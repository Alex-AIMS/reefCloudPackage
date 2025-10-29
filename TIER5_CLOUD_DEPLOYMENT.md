# Tier 5 Cloud Deployment Guide

## Overview

Since AIMS HPC is limited to 80GB RAM and Tier 5 requires 128GB minimum, this guide shows how to run Tier 5 analysis on cloud infrastructure.

## Cost Estimate

| Provider | Instance Type | RAM | vCPUs | Cost/Hour | 48h Cost |
|----------|--------------|-----|-------|-----------|----------|
| **AWS** | r6i.4xlarge | 128GB | 16 | $2.52 | **$121** |
| **AWS** | r6i.8xlarge | 256GB | 32 | $5.04 | $242 |
| **Azure** | Standard_E16_v5 | 128GB | 16 | $2.42 | **$116** |
| **GCP** | n2-highmem-16 | 128GB | 16 | $2.50 | **$120** |

**Recommended**: Start with 128GB instance. Upgrade to 256GB only if OOM occurs.

## Option 1: AWS EC2 (Recommended)

### Step 1: Launch Instance

```bash
# Install AWS CLI if not already installed
pip install awscli

# Configure AWS credentials
aws configure

# Launch instance
aws ec2 run-instances \
  --image-id ami-0c55b159cbfafe1f0 \  # Ubuntu 22.04 (check latest AMI)
  --instance-type r6i.4xlarge \
  --key-name your-keypair \
  --security-group-ids sg-xxxxxxxx \
  --subnet-id subnet-xxxxxxxx \
  --block-device-mappings '[{"DeviceName":"/dev/sda1","Ebs":{"VolumeSize":200,"VolumeType":"gp3"}}]' \
  --tag-specifications 'ResourceType=instance,Tags=[{Key=Name,Value=reefcloud-tier5}]'

# Note the instance ID from output
INSTANCE_ID=i-xxxxxxxxxx

# Get public IP
aws ec2 describe-instances --instance-ids $INSTANCE_ID \
  --query 'Reservations[0].Instances[0].PublicIpAddress' --output text
```

### Step 2: Setup Instance

```bash
# SSH to instance
ssh -i your-keypair.pem ubuntu@<PUBLIC_IP>

# Update system
sudo apt-get update
sudo apt-get upgrade -y

# Install Docker
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh
sudo usermod -aG docker ubuntu

# Or install Singularity
sudo apt-get install -y software-properties-common
sudo add-apt-repository -y ppa:apptainer/ppa
sudo apt-get update
sudo apt-get install -y apptainer

# Logout and login again for group changes
exit
ssh -i your-keypair.pem ubuntu@<PUBLIC_IP>
```

### Step 3: Transfer Data and Image

```bash
# From your local machine, transfer Singularity image
scp -i your-keypair.pem reefcloud_optimised_v1.tar.gz ubuntu@<PUBLIC_IP>:~/

# Transfer data (if not already on cloud storage)
rsync -avz -e "ssh -i your-keypair.pem" \
  /path/to/local/data/ \
  ubuntu@<PUBLIC_IP>:~/reefcloud_data/

# Or transfer from AIMS HPC
ssh azivalje@hpc-l001 "cd /scratch/\${USER} && tar czf - reefcloud_data" | \
  ssh -i your-keypair.pem ubuntu@<PUBLIC_IP> "tar xzf - -C ~/"
```

### Step 4: Run Analysis on EC2

```bash
# SSH to EC2
ssh -i your-keypair.pem ubuntu@<PUBLIC_IP>

# Convert and load Singularity image
gunzip reefcloud_optimised_v1.tar.gz
singularity build reefcloud_optimised_v1.sif docker-archive://reefcloud_optimised_v1.tar

# Run analysis with screen (so it continues if SSH disconnects)
screen -S tier5analysis

# Run Tier 5 analysis
singularity exec \
  --bind ~/reefcloud_data:/input-data \
  --bind ~/output:/output-data \
  --pwd /home/project \
  --cleanenv \
  reefcloud_optimised_v1.sif \
  Rscript -e "
    args <- c(
        '--bucket=/input-data/',
        '--domain=tier',
        '--by_tier=5',
        '--model_type=6',
        '--debug=true',
        '--runStage=-1',
        '--refresh_data=false'
    )
    reefCloudPackage::startMatter(args)
    reefCloudPackage::model_loadData()
    reefCloudPackage::model_processData()
    reefCloudPackage::model_fitModel()
  " 2>&1 | tee tier5_analysis.log

# Detach from screen: Ctrl+A, then D
# Reattach later: screen -r tier5analysis
```

### Step 5: Monitor Progress

```bash
# SSH to EC2
ssh -i your-keypair.pem ubuntu@<PUBLIC_IP>

# Monitor log
tail -f tier5_analysis.log

# Check memory usage
watch -n 60 'free -h'

# Reattach to screen
screen -r tier5analysis
```

### Step 6: Retrieve Results

```bash
# From local machine, copy results
rsync -avz -e "ssh -i your-keypair.pem" \
  ubuntu@<PUBLIC_IP>:~/output/ \
  ./tier5_results/

# Or copy to AIMS HPC
ssh -i your-keypair.pem ubuntu@<PUBLIC_IP> "cd ~/output && tar czf - ." | \
  ssh azivalje@hpc-l001 "cd /scratch/\${USER} && tar xzf - -C reefcloud_output_tier5/"
```

### Step 7: Terminate Instance

```bash
# Stop instance (can restart later)
aws ec2 stop-instances --instance-ids $INSTANCE_ID

# Or terminate (delete forever) - do this after copying results!
aws ec2 terminate-instances --instance-ids $INSTANCE_ID

# Verify termination
aws ec2 describe-instances --instance-ids $INSTANCE_ID
```

## Option 2: Azure VM

### Launch VM

```bash
# Install Azure CLI
curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash

# Login
az login

# Create resource group
az group create --name reefcloud-rg --location australiaeast

# Create VM
az vm create \
  --resource-group reefcloud-rg \
  --name reefcloud-tier5 \
  --image Ubuntu2204 \
  --size Standard_E16_v5 \
  --admin-username azureuser \
  --generate-ssh-keys \
  --public-ip-sku Standard

# Get public IP
az vm show --resource-group reefcloud-rg --name reefcloud-tier5 \
  --show-details --query publicIps --output tsv
```

### Run Analysis

Follow same steps as AWS (Step 2-6), but use Azure VM's IP address.

### Cleanup

```bash
# Delete resource group (includes VM and all resources)
az group delete --name reefcloud-rg --yes --no-wait
```

## Option 3: Google Cloud Platform

### Launch VM

```bash
# Install gcloud CLI
curl https://sdk.cloud.google.com | bash

# Initialize
gcloud init

# Create VM
gcloud compute instances create reefcloud-tier5 \
  --zone=australia-southeast1-a \
  --machine-type=n2-highmem-16 \
  --image-family=ubuntu-2204-lts \
  --image-project=ubuntu-os-cloud \
  --boot-disk-size=200GB

# Get external IP
gcloud compute instances describe reefcloud-tier5 \
  --zone=australia-southeast1-a \
  --format='get(networkInterfaces[0].accessConfigs[0].natIP)'
```

### Run Analysis

Follow same steps as AWS (Step 2-6).

### Cleanup

```bash
# Delete VM
gcloud compute instances delete reefcloud-tier5 \
  --zone=australia-southeast1-a
```

## Option 4: Docker on Cloud (Simpler)

If using Docker instead of Singularity:

```bash
# On EC2/Azure/GCP instance
docker load -i reefcloud_optimised_v1.tar

# Run analysis
docker run --rm \
  -v ~/reefcloud_data:/input-data \
  -v ~/output:/output-data \
  --memory=120g \
  reefcloud:optimised_v1 \
  Rscript -e "
    args <- c(
        '--bucket=/input-data/',
        '--domain=tier',
        '--by_tier=5',
        '--model_type=6',
        '--debug=true',
        '--runStage=-1',
        '--refresh_data=false'
    )
    reefCloudPackage::startMatter(args)
    reefCloudPackage::model_loadData()
    reefCloudPackage::model_processData()
    reefCloudPackage::model_fitModel()
  "
```

## Cost Optimization Tips

### 1. Use Spot/Preemptible Instances

**AWS Spot Instances** (60-90% discount):
```bash
aws ec2 request-spot-instances \
  --instance-count 1 \
  --type "one-time" \
  --launch-specification file://spot-spec.json
```

**Risk**: Instance can be terminated if demand increases. Use with checkpointing.

### 2. Use Reserved Instances

If running multiple analyses, reserve an instance for 1-3 years (up to 75% discount).

### 3. Stop, Don't Terminate

Keep instance stopped between runs to preserve setup:
```bash
# Stop (keeps data, can restart)
aws ec2 stop-instances --instance-ids $INSTANCE_ID

# Start again later
aws ec2 start-instances --instance-ids $INSTANCE_ID
```

**Cost while stopped**: Only storage (~$20/month for 200GB)

### 4. Use Object Storage

Store data in S3/Azure Blob/GCS between runs:
```bash
# Upload to S3
aws s3 sync ~/reefcloud_data s3://my-bucket/reefcloud_data/

# Download when needed
aws s3 sync s3://my-bucket/reefcloud_data/ ~/reefcloud_data/
```

**Cost**: ~$2/month for 100GB

## Comparison: Cloud vs HPC High-Memory

| Factor | Cloud (128GB) | AIMS High-Mem (if available) |
|--------|---------------|------------------------------|
| Cost | $100-150 | Free (HPC allocation) |
| Setup Time | 30 min | Depends on approval |
| Availability | Immediate | May require approval |
| Control | Full root access | Limited user access |
| Flexibility | Any instance size | Fixed node specs |
| **Recommendation** | **Use while waiting for HPC approval** | **Use if available** |

## Troubleshooting

### Out of Disk Space

```bash
# Check disk usage
df -h

# Clean Docker/Singularity cache
docker system prune -a
# or
singularity cache clean --all

# Increase EBS volume (AWS)
aws ec2 modify-volume --volume-id vol-xxxxx --size 300
# Then resize filesystem:
sudo growpart /dev/xvda 1
sudo resize2fs /dev/xvda1
```

### Still Getting OOM on 128GB

```bash
# Try 256GB instance
aws ec2 run-instances --instance-type r6i.8xlarge ...

# Or use Model Type 5
# In R script: '--model_type=5'
```

### Slow Data Transfer

```bash
# Use compression
tar czf - data/ | ssh ubuntu@<IP> "tar xzf - -C ~/"

# Or use AWS DataSync / Azure Data Box / GCP Transfer Service
# for very large datasets (>1TB)
```

## Complete Workflow Script

```bash
#!/bin/bash
# tier5_cloud_workflow.sh

set -e

KEYPAIR="your-keypair"
INSTANCE_TYPE="r6i.4xlarge"

echo "=== Step 1: Launch Instance ==="
INSTANCE_ID=$(aws ec2 run-instances \
  --image-id ami-0c55b159cbfafe1f0 \
  --instance-type $INSTANCE_TYPE \
  --key-name $KEYPAIR \
  --query 'Instances[0].InstanceId' \
  --output text)

echo "Instance ID: $INSTANCE_ID"
echo "Waiting for instance to be running..."
aws ec2 wait instance-running --instance-ids $INSTANCE_ID

PUBLIC_IP=$(aws ec2 describe-instances --instance-ids $INSTANCE_ID \
  --query 'Reservations[0].Instances[0].PublicIpAddress' --output text)

echo "Public IP: $PUBLIC_IP"

echo "=== Step 2: Wait for SSH ==="
sleep 60

echo "=== Step 3: Setup Instance ==="
ssh -o StrictHostKeyChecking=no -i ${KEYPAIR}.pem ubuntu@${PUBLIC_IP} << 'EOF'
  curl -fsSL https://get.docker.com | sh
  sudo usermod -aG docker ubuntu
EOF

echo "=== Step 4: Transfer Data ==="
scp -i ${KEYPAIR}.pem reefcloud_optimised_v1.tar ubuntu@${PUBLIC_IP}:~/
rsync -avz -e "ssh -i ${KEYPAIR}.pem" reefcloud_data/ ubuntu@${PUBLIC_IP}:~/data/

echo "=== Step 5: Run Analysis ==="
ssh -i ${KEYPAIR}.pem ubuntu@${PUBLIC_IP} << 'EOF'
  docker load -i reefcloud_optimised_v1.tar
  docker run --rm -v ~/data:/input-data -v ~/output:/output-data \
    reefcloud:optimised_v1 Rscript -e "..." > tier5.log 2>&1
EOF

echo "=== Step 6: Retrieve Results ==="
rsync -avz -e "ssh -i ${KEYPAIR}.pem" ubuntu@${PUBLIC_IP}:~/output/ ./results/

echo "=== Step 7: Terminate Instance ==="
aws ec2 terminate-instances --instance-ids $INSTANCE_ID

echo "COMPLETE!"
```

## Summary

**For Tier 5 with 128GB requirement:**

1. ✅ **Best**: Use cloud VM (immediate, ~$120 for 48h)
2. ✅ **Also Good**: Request AIMS HPC high-memory node (free but requires approval)
3. ⚠️ **Worth Trying**: Optimize to fit 80GB (low success rate)

**Cloud deployment provides**:
- Immediate availability
- Guaranteed resources
- Full control
- Pay only for what you use
- Can be automated

**Total Cost**: ~$100-150 for one Tier 5 analysis run

This is often cheaper than the time spent trying to optimize for insufficient resources!
