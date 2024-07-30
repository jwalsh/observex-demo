#!/bin/bash

# Create the main project directory
mkdir -p internal-developer-platform
cd internal-developer-platform

# Create directories for teams
mkdir -p teams/{order-fulfillment,product-catalog,customer-experience}

# Create platform directory
mkdir -p platform/{api-hub,shared-models,monitoring}

# Create innersource directory owned by platform
mkdir -p platform/innersource

# Create distributed tracing directory within innersource
mkdir -p platform/innersource/distributed-tracing/{core,python,typescript,clojure,guile}

# Create subdirectories for each language implementation
for lang in python typescript clojure guile
do
    mkdir -p platform/innersource/distributed-tracing/$lang/{src,tests,examples,docs,publish}
    touch platform/innersource/distributed-tracing/$lang/README.org

    # Add language-specific publish tooling
    case $lang in
        python)
            touch platform/innersource/distributed-tracing/$lang/publish/setup.py
            ;;
        typescript)
            touch platform/innersource/distributed-tracing/$lang/publish/package.json
            ;;
        clojure)
            touch platform/innersource/distributed-tracing/$lang/publish/project.clj
            ;;
        guile)
            touch platform/innersource/distributed-tracing/$lang/publish/Makefile
            ;;
    esac
done

# Create shared directory for common components
mkdir -p shared/{graphql,monitoring}

# Create directories for each team
for team in order-fulfillment product-catalog customer-experience
do
    mkdir -p teams/$team/{services,ui,database,infrastructure,deployment,docs}

    # Create subdirectories for services
    mkdir -p teams/$team/services/{src,tests,config}

    # Create subdirectories for UI
    mkdir -p teams/$team/ui/{src,tests,public}

    # Create subdirectories for database
    mkdir -p teams/$team/database/{schemas,migrations,scripts}

    # Create subdirectories for infrastructure
    mkdir -p teams/$team/infrastructure/{terraform,kubernetes}

    # Create subdirectories for deployment
    mkdir -p teams/$team/deployment/{pipelines,scripts,config}

    # Create team-specific docs directories
    mkdir -p teams/$team/docs/{architecture,api,runbooks}
done

# Create specific service directories
mkdir -p teams/order-fulfillment/services/order-service
mkdir -p teams/product-catalog/services/product-service
mkdir -p teams/customer-experience/services/{customer-service,payment-service}

# Create platform-specific directories
mkdir -p platform/api-hub/{schemas,documentation}
mkdir -p platform/shared-models/{order,product,customer}
mkdir -p platform/monitoring/{prometheus,grafana,alerts,dashboards}

# Create shared GraphQL directory
mkdir -p shared/graphql/{schema,resolvers}

# Create a directory for examples of using the innersource distributed tracing
mkdir -p examples/distributed-tracing

# Create README files in org-mode
echo "#+TITLE: Internal Developer Platform
#+AUTHOR: Platform Team

* Overview
This is the root directory of our Internal Developer Platform.

* Structure
- teams/: Team-specific services and components
- platform/: Shared platform services and innersource components
- shared/: Common utilities and configurations
- examples/: Usage examples for platform components" > README.org

echo "#+TITLE: Innersource Distributed Tracing
#+AUTHOR: Platform Team

* Overview
This directory contains the innersource distributed tracing implementations.

* Supported Languages
- Python
- TypeScript
- Clojure
- Guile Scheme

* Usage
Refer to the language-specific directories for implementation details and usage instructions." > platform/innersource/distributed-tracing/README.org

echo "#+TITLE: Distributed Tracing Usage Examples
#+AUTHOR: Platform Team

* Overview
This directory contains examples of how to use the innersource distributed tracing implementations in various languages and scenarios." > examples/distributed-tracing/README.org

echo "Directory structure for Internal Developer Platform has been created."