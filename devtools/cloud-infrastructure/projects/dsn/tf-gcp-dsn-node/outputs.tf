/**
 * Copyright 2019 Google LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

output "dsn-ips" {
  description = "The instances created by the module"
  value       = { for vm in google_compute_instance.dsn_node : vm.name => { "external_ip" = vm.network_interface.0.access_config.0.nat_ip, "name" = vm.name, "zone" = vm.zone } }
}

output "service_account" {
  value       = google_service_account.dsn_nodes_service_account
  description = "The service account used by DSN nodes"
}
