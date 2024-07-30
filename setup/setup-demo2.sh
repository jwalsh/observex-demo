#!/bin/bash

# Create the main project directory
mkdir -p internal-developer-platform
cd internal-developer-platform

# ... [Previous README creation code remains unchanged] ...

# Create platform directories
mkdir -p platform/{api-hub,shared-models,innersource/{distributed-tracing,shared-models},terraform-modules,change-management,shared-infrastructure}

# Create change management subdirectories
mkdir -p platform/change-management/{tooling,requests,policies}

# Create shared infrastructure subdirectories
mkdir -p platform/shared-infrastructure/{aws,kubernetes}

# ... [Previous shared and examples directory creation code remains unchanged] ...

# Function to create team structure
create_team_structure() {
    local team_name=$1
    mkdir -p teams/$team_name/{services,ui,database,infrastructure/{terraform,kubernetes},deployment,monitoring,security,onboarding,testing,docs}
}

# Create team directories
create_team_structure "order-fulfillment"
create_team_structure "product-catalog"
create_team_structure "customer-experience"

# Create platform team directory
mkdir -p platform/engineering

# Create some example files
touch platform/api-hub/README.md
touch platform/shared-models/README.md
touch platform/innersource/distributed-tracing/README.md
touch platform/innersource/shared-models/README.md

# Create change management files
touch platform/change-management/tooling/change-control-tool-config.yml
touch platform/change-management/requests/change-request-template.md
touch platform/change-management/policies/change-management-policy.md

# Create shared infrastructure files
touch platform/shared-infrastructure/aws/organization-structure.tf
touch platform/shared-infrastructure/aws/shared-vpc.tf
touch platform/shared-infrastructure/kubernetes/cluster-config.yml
touch platform/shared-infrastructure/kubernetes/resource-quotas.yml

# ... [Previous team checklist creation code remains unchanged] ...

# Create additional team-specific files based on their responses
# ... [Previous team-specific file creation code remains unchanged] ...

# Create platform engineering specific files
touch platform/engineering/platform-architecture.md
touch platform/engineering/security-policy.md
touch platform/engineering/observability-standards.md
touch platform/engineering/change-control-process.md

cat << EOF > platform/change-management/README.org
#+TITLE: Change Management
#+AUTHOR: Platform Engineering Team

* Overview
This directory contains all change management related components for our Internal Developer Platform.

* Structure
- tooling/: Configuration for change control tools
- requests/: Templates and storage for change requests
- policies/: Change management policies and procedures

* Usage
Refer to the change-management-policy.md file for detailed information on our change control process.
EOF

cat << EOF > platform/shared-infrastructure/README.org
#+TITLE: Shared Infrastructure
#+AUTHOR: Platform Engineering Team

* Overview
This directory contains shared infrastructure configurations for AWS and Kubernetes.

* Structure
- aws/: Shared AWS configurations, including organization structure and shared resources
- kubernetes/: Shared Kubernetes configurations, including cluster setup and resource quotas

* Usage
These configurations should be used as the base for all team-specific infrastructure. Any changes to these shared configurations must go through the change control process.
EOF

echo "Internal Developer Platform repository structure has been created with team-specific IDP checklists, change management components, and shared infrastructure configurations."