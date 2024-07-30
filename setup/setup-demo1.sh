#!/bin/bash

# Create the main project directory
mkdir -p internal-developer-platform
cd internal-developer-platform

# Create README file
cat << EOF > README.org
#+TITLE: Internal Developer Platform
#+AUTHOR: Platform Team

* Overview
This is the root directory of our Internal Developer Platform.

* Structure
- platform/: Shared platform services and innersource components
- teams/: Team-specific services and components
- shared/: Common utilities and configurations
- examples/: Usage examples for platform components
EOF

# Create platform directories
mkdir -p platform/{api-hub,shared-models,innersource/{distributed-tracing,shared-models},terraform-modules}

# Create shared directories
mkdir -p shared/{ui-components,utilities}

# Create examples directories
mkdir -p examples/{distributed-tracing,shared-models,local-first-apps/{kubernetes-local,aws-services-local}}

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

# Create team-specific IDP checklist files
create_team_checklist() {
    local team_name=$1
    local checklist_file="teams/$team_name/idp-checklist-2024.org"

    cat << EOF > "$checklist_file"
#+TITLE: $team_name Team IDP Checklist 2024
#+AUTHOR: $team_name Team

* IDP Maturity Checklist for $team_name Team

** Architecture and Design
- [ ] Ensure architecture aligns with IDP guidelines
- [ ] Review and approve service interaction patterns
- [ ] Validate scalability and performance designs

** Code Quality and Standards
- [ ] Enforce use of shared code quality tools
- [ ] Regularly review and update coding standards
- [ ] Ensure proper implementation of distributed tracing

** Security
- [ ] Implement and review security practices
- [ ] Ensure compliance with data protection regulations
- [ ] Regularly conduct security audits

** Infrastructure and Deployment
- [ ] Utilize IaC for all infrastructure components
- [ ] Implement and maintain CI/CD pipelines
- [ ] Ensure proper use of Kubernetes for containerized applications

** Monitoring and Observability
- [ ] Set up comprehensive monitoring for services
- [ ] Establish and track SLIs/SLOs for critical processes
- [ ] Implement effective logging and tracing

** Local-First Development
- [ ] Ensure local development environment mirrors production
- [ ] Implement effective use of LocalStack for AWS service emulation
- [ ] Maintain offline-capable features for critical functions

[... Additional sections as per team-specific requirements ...]

EOF

    echo "Created IDP checklist for $team_name team"
}

create_team_checklist "order-fulfillment"
create_team_checklist "product-catalog"
create_team_checklist "customer-experience"

# Create platform engineering team checklist
cat << EOF > platform/engineering/idp-checklist-2024.org
#+TITLE: Platform Engineering Team IDP Checklist 2024
#+AUTHOR: Platform Engineering Team

* IDP Maturity Checklist for Platform Engineering Team

** Architecture and Design
- [ ] Design and maintain overall IDP architecture
- [ ] Establish platform-wide design principles and best practices
- [ ] Ensure scalability and performance of platform services

** Code Quality and Standards
- [ ] Define and enforce platform-wide coding standards
- [ ] Implement and maintain shared libraries and SDKs
- [ ] Ensure proper implementation of observability standards

** Security
- [ ] Design and implement platform-wide security architecture
- [ ] Ensure compliance with industry standards and regulations
- [ ] Conduct regular security audits and penetration testing

[... Additional sections as per platform team-specific requirements ...]

EOF

echo "Created IDP checklist for Platform Engineering team"

# Create additional team-specific files based on their responses
touch teams/order-fulfillment/services/order-service.java
touch teams/order-fulfillment/infrastructure/terraform/main.tf
touch teams/order-fulfillment/monitoring/prometheus-config.yml

touch teams/product-catalog/services/catalog-service.java
touch teams/product-catalog/database/elasticsearch-schema.json
touch teams/product-catalog/infrastructure/kubernetes/deployment.yaml

touch teams/customer-experience/ui/customer-portal.js
touch teams/customer-experience/services/customer-service.js
touch teams/customer-experience/monitoring/grafana-dashboard.json

touch platform/engineering/platform-architecture.md
touch platform/engineering/security-policy.md
touch platform/engineering/observability-standards.md

echo "Internal Developer Platform repository structure has been created with team-specific IDP checklists and example files."