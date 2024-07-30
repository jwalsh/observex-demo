#!/bin/bash

# Create main directories
mkdir -p internal-developer-platform/{platform,teams,shared,examples}

# Create platform directories
mkdir -p internal-developer-platform/platform/{api-hub,monitoring,logging,cicd,service-mesh,feature-flags,secrets-management,gitops,telepresence}

# Create team directories
for team in customer-experience order-fulfillment product-catalog
do
    mkdir -p internal-developer-platform/teams/$team/{services,ui,database,infrastructure,deployment,docs}
done

# Create shared directories
mkdir -p internal-developer-platform/shared/{graphql-federation,schema-registry}

# Create example directories
mkdir -p internal-developer-platform/examples/local-development

# Create README files with descriptions of new components
echo "# Service Mesh
This directory contains configurations and documentation for our service mesh implementation.
It enhances inter-service communication and security." > internal-developer-platform/platform/service-mesh/README.md

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

echo "Directory structure for Internal Developer Platform has been created with new recommended components."