#!/bin/bash

# Create main directories
mkdir -p internal-developer-platform/{platform,teams,frontend,docs,demo}

# Create frontend directories
mkdir -p internal-developer-platform/frontend/{static-assets,bff}

# Create platform directories
mkdir -p internal-developer-platform/platform/{api-gateway,service-mesh,service-discovery,config-management,secret-management,observability,cicd,feature-flags,gitops}

# Create observability subdirectories
mkdir -p internal-developer-platform/platform/observability/{monitoring,logging,tracing}

# Create team directories
for team in customer-experience order-fulfillment product-catalog
do
    mkdir -p internal-developer-platform/teams/$team
done

# Create data layer subdirectories
mkdir -p internal-developer-platform/data-layer/{cache,primary-db,data-warehouse,message-queue,event-bus}

# Create demo directories
mkdir -p internal-developer-platform/demo/distributed-tracing

# Remove directories that are no longer needed
rm -rf internal-developer-platform/platform/{api-hub,telepresence,service-registry}
rm -rf internal-developer-platform/shared
rm -rf internal-developer-platform/examples
rm -rf internal-developer-platform/frontend/{cdn,proxy}

# Update README files
echo "# Frontend Proxy / Load Balancer
This directory contains configurations for our Frontend Proxy and Load Balancer.
It routes requests to static assets or the Backend for Frontend (BFF)." > internal-developer-platform/frontend/README.md

echo "# Static Assets
This directory contains static assets served by the Frontend Proxy." > internal-developer-platform/frontend/static-assets/README.md

echo "# Backend for Frontend (BFF)
This directory contains implementations of our Backend for Frontend services.
These API layers are specific to frontend needs and communicate with the API Gateway." > internal-developer-platform/frontend/bff/README.md

echo "# API Gateway
This directory contains configurations for our API Gateway.
It handles authentication, authorization, and rate limiting for incoming API requests." > internal-developer-platform/platform/api-gateway/README.md

echo "# Service Mesh
This directory contains configurations for our Service Mesh.
It manages service-to-service communication, including service discovery and load balancing." > internal-developer-platform/platform/service-mesh/README.md

echo "# Observability
This directory contains our observability stack, including monitoring, logging, and distributed tracing tools." > internal-developer-platform/platform/observability/README.md

echo "# Feature Flags
This directory contains our feature flag management system configurations." > internal-developer-platform/platform/feature-flags/README.md

echo "# Data Layer
This directory contains configurations for our data infrastructure, including caching, databases, and message queues." > internal-developer-platform/data-layer/README.md

echo "Directory structure for Internal Developer Platform has been updated to reflect the current architecture."