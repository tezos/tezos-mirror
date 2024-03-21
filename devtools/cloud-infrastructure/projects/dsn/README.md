# Decentralised Sequencer Network - Infrastructure for Dev environment

This directory contains several terraform modules to bootstrap a decentralised sequencer network on GCP. 
A decentralised sequencer network consists of a sequencer node, and several keyholder nodes. These nodes are distributed and  
Users can submit encrypted transactions to the sequencer, and keyholders reveal a decryption share 
for the transaction. The transaction can be decrypted only after a number of keyholders have 
revealed their decryption share. 

The directory contains the following modules: 
- tf-gcp-state-storage-bucket: creates the gcp bucket where the terraform state for other modules will be saved,
- tf-gcp-docker-registry: creates a docker registry that can be used to upload the sequencer Docker images,
- tf-gcp-network: creates a network with a subnetwork in each user-specified regions. Route GBP propagation is disabled, to simulate a realistic environment where the traffic is routed through the internet,
- tf-gcp-dsn-node: Creates a VM in each of the regions of the network, and downloads the specified docker image from the docker registry upon start. 

These terraform modules are being used for benchmarking the Sequencer with Threshold Encryption.
If you want to reuse them in your GCP project, change the value for `project_id` in each module's `variables.tf` file. 
Each module can be initialized with the command `terraform init`. 
An initialized module can be applied with `terraform apply` (will require to type `yes` after the terminal lists what resources will be created). 
You can destroy applied resources with `terraform destroy` (will require to type `yes` after the terminal lists what resources will be destroyed).

It is sufficient to initialize each module only once. The modules `tf-gcp-state-storage-bucket` and `tf-gcp-docker-registry` will only need to be applied once (in that order).
To create a DSN network, you can apply the `tf-gcp-network` module. Here you can specify the regions that the network will span onto via the `variables.tf` file.
To create the VM machines in the network, you can apply the `tf-gcp-dsn-node` module. Here you can specify the type of machine and the docker images to be downloaded when the machine starts, via the `variable.tf` file. 
