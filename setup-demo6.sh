#!/bin/bash

# Create main directories
mkdir -p internal-developer-platform/{platform,teams,shared,examples,frontend}

# Create frontend directories
mkdir -p internal-developer-platform/frontend/{cdn,bff,proxy}

# Create platform directories
mkdir -p internal-developer-platform/platform/{api-hub,api-gateway,service-mesh,service-registry,monitoring,logging,cicd,feature-flags,secrets-management,gitops,telepresence}

# Create team directories
for team in customer-experience order-fulfillment product-catalog
do
    mkdir -p internal-developer-platform/teams/$team/{services,ui,database,infrastructure,deployment,docs}
done

# Create shared directories
mkdir -p internal-developer-platform/shared/graphql-federation

# Create example directories
mkdir -p internal-developer-platform/examples/local-development

# Create README files with descriptions of new components
echo "# Frontend Proxy
This directory contains configurations for our Frontend Proxy.
It routes requests to either static content or the API Gateway and handles HTTPS termination." > internal-developer-platform/frontend/proxy/README.md

echo "# CDN / Static File Server
This directory contains configurations for our CDN or static file server.
It serves static assets like HTML, CSS, and JavaScript files." > internal-developer-platform/frontend/cdn/README.md

echo "# Backend for Frontend (BFF)
This directory contains implementations of our Backend for Frontend services.
These lightweight API layers are specific to each frontend application and optimize data for UI needs." > internal-developer-platform/frontend/bff/README.md

echo "# API Gateway
This directory contains configurations for our API Gateway.
It acts as the entry point for API requests, handling authentication, rate limiting, and request routing." > internal-developer-platform/platform/api-gateway/README.md

echo "# Service Mesh
This directory contains configurations and documentation for our service mesh implementation.
It focuses on internal service-to-service communication, providing service discovery, load balancing, and security." > internal-developer-platform/platform/service-mesh/README.md

echo "# Service Registry
This directory contains configurations for our Service Registry.
It maintains a catalog of all internal services, supporting both the Service Mesh and API Gateway." > internal-developer-platform/platform/service-registry/README.md

echo "Directory structure for Internal Developer Platform has been created with updated frontend components."