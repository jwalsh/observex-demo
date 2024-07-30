#!/bin/bash

# Create main directories
mkdir -p internal-developer-platform/{platform,teams,shared,examples}

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

# Create README files with descriptions of components
echo "# API Hub
This directory contains configurations and documentation for our API Hub.
It focuses on external-facing APIs, including documentation, versioning, and governance." > internal-developer-platform/platform/api-hub/README.md

echo "# API Gateway
This directory contains configurations for our API Gateway.
It acts as the entry point for external requests, handling authentication, rate limiting, and request routing." > internal-developer-platform/platform/api-gateway/README.md

echo "# Service Mesh
This directory contains configurations and documentation for our service mesh implementation.
It focuses on internal service-to-service communication, providing service discovery, load balancing, and security." > internal-developer-platform/platform/service-mesh/README.md

echo "# Service Registry
This directory contains configurations for our Service Registry.
It maintains a catalog of all internal services, supporting both the Service Mesh and API Gateway." > internal-developer-platform/platform/service-registry/README.md

echo "# Feature Flags
This directory contains our feature flag management system configurations.
It enables easier rollouts and A/B testing capabilities." > internal-developer-platform/platform/feature-flags/README.md

echo "# Secrets Management
This directory contains configurations for our central secrets management solution.
It provides secure storage and access to sensitive information." > internal-developer-platform/platform/secrets-management/README.md

echo "# GitOps
This directory contains our GitOps configurations and practices.
It enables declarative infrastructure and application deployments." > internal-developer-platform/platform/gitops/README.md

echo "# Telepresence
This directory contains configurations and documentation for Telepresence.
It enhances local development experience for microservices." > internal-developer-platform/platform/telepresence/README.md

echo "Directory structure for Internal Developer Platform has been created with updated components."