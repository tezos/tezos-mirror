- Install Locust (V2.32.5) using pip install locust or sudo apt-get install python3-locust.
- Run the following command: 
  `dune exec src/bin_testnet_scenarios/main.exe -- --file evm_rollup.ml rps -v`
- A folder named with the timestamp (e.g., 2024-12-13T10:11:16-00:00) will be created in the Locust directory. This folder contains the results of the Tezt.
- You can tune the Locust setup in Tezt:
    - Time to execution.
    - Number of users sending RPC requests.