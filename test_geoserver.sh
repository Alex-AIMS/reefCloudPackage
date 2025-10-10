#!/bin/bash
# Test script for geoserver WFS requests
# This mimics the requests made by R code in get_geoserver_data.R

GEOSERVER_URL="https://geoserver.apps.aims.gov.au/reefcloud/ows"

echo "=========================================="
echo "Testing Geoserver WFS Access"
echo "=========================================="
echo ""
echo "Geoserver URL: $GEOSERVER_URL"
echo ""

# Test 1: GetCapabilities request (basic connectivity)
echo "Test 1: GetCapabilities request"
echo "---"
curl -v -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36" \
     -H "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8" \
     -H "Accept-Language: en-US,en;q=0.9" \
     -H "Accept-Encoding: gzip, deflate, br" \
     -H "Connection: keep-alive" \
     "${GEOSERVER_URL}?service=WFS&version=1.0.0&request=GetCapabilities" \
     2>&1 | head -50
echo ""
echo ""

# Test 2: GetFeature request for DHW (Degree Heating Weeks) - Tier 4
echo "Test 2: GetFeature - Degree Heating Weeks (Tier 4)"
echo "---"
# Using a bounding box for Australian waters
BBOX="110.0,-45.0,155.0,-10.0"
curl -v -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36" \
     -H "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8" \
     -H "Accept-Language: en-US,en;q=0.9" \
     -H "Accept-Encoding: gzip, deflate, br" \
     -H "Connection: keep-alive" \
     "${GEOSERVER_URL}?service=WFS&version=1.0.0&request=GetFeature&typeName=reefcloud:degrees_heating_weeks_tier&srsName=EPSG:4326&bbox=${BBOX}" \
     2>&1 | head -50
echo ""
echo ""

# Test 3: GetFeature request for Cyclone exposure - Tier 4
echo "Test 3: GetFeature - Cyclone Exposure (Tier 4)"
echo "---"
curl -v -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36" \
     -H "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8" \
     -H "Accept-Language: en-US,en;q=0.9" \
     -H "Accept-Encoding: gzip, deflate, br" \
     -H "Connection: keep-alive" \
     "${GEOSERVER_URL}?service=WFS&version=1.0.0&request=GetFeature&typeName=reefcloud:storm4m_exposure_year_tier&srsName=EPSG:4326&bbox=${BBOX}" \
     2>&1 | head -50
echo ""
echo ""

echo "=========================================="
echo "Tests Complete"
echo "=========================================="
echo ""
echo "Look for HTTP status codes in the output above:"
echo "  - 200 OK = Success"
echo "  - 403 Forbidden = Access denied"
echo "  - 404 Not Found = Layer doesn't exist"
echo ""
