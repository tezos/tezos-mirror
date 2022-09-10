function range(start, end) {
    var ans = [];
    for (let i = start; i <= end; i++) {
        ans.push(+i);
    }
    return ans;
}

function onlyUnique(value, index, self) {
    return self.indexOf(value) === index;
}

function populate_v1(server_adress, beg, end) { //dès lors qu'on aura accès au nouveau serveur 
    let dict_data = {};
    var range_bloc = range(beg, end);
    return Promise.all(
        range_bloc.map((height) => {
            return axios
                .get(server_adress + height + ".json")
                .then(json_data => {
                    dict_data[height] = json_data.data;
                }, error => { console.log(error) })

        })).then(_ => { return dict_data })
}

const populate_v0 = function (server, beg, end, directories, dict_data) {
    var range_bloc = range(beg, end);
    range_bloc.forEach((a) => {
        Object.entries(directories).forEach(([k, v]) => {
            if ((+a) <= v[1] && (+a) >= v[0]) {
                try {
                    axios
                        .get(server + "/" + k + "/" + a + ".json")
                        .then(json_data => {
                            dict_data[a] = json_data.data;
                        })
                } catch {
                    console.log("bloc: " + a + " est introuvable");
                }
            }
        })
    })
    return dict_data
}

function get_info_bloc(dict_data) {
    let t_delai_bloc = {};
    let max_round = 0;
    let t_baker = {}
    Object.entries(dict_data).forEach(([va, v]) => {
        if ("blocks" in v) {
            v["blocks"].forEach((element) => {
                let round = 0;
                if ("round" in element) round = element["round"];
                if (("reception_time" in element) && ("timestamp" in element)) t_delai_bloc[round] = new Date(new Date(element["reception_time"]) - new Date(element["timestamp"])).getSeconds();
                if ("timestamp" in element) t_baker[round] = new Date(element["timestamp"]);
                if (round > max_round) {
                    max_round = round
                }
            });
        }
    });
    return [t_delai_bloc, t_baker, max_round]
}
function delegate_delays_distribution_of_operations(dict_data, delegate) {
    let t_valide = []
    let complete_delays = delays_distribution_of_operations(dict_data)
    let delays_endorsement = complete_delays[1]
    let delays_pre_endorsement = complete_delays[0]
    Object.entries(dict_data).forEach(([va, v]) => {
        let t_baker = {};
        if ("blocks" in v) {
            v["blocks"].forEach((element) => {
                let round = 0;
                if ("round" in element) round = element["round"];
                if ("timestamp" in element) t_baker[round] = new Date(element["timestamp"]);
            })
        }
        Object.entries(v["endorsements"]).forEach(([_, baker_ops]) => {
            if (baker_ops["delegate"] == delegate) {
                if ("operations" in baker_ops)
                    baker_ops["operations"].forEach((operation) => {
                        let round_cib = 0;
                        if ("round" in operation) round_cib = operation["round"];
                        if ((round_cib in t_baker) && ("reception_time" in operation) && (operation["reception_time"] != null)) {
                            let delay = new Date(new Date(operation["reception_time"]) - t_baker[round_cib]).getSeconds();
                            if (("kind" in operation) && (va in delays_pre_endorsement) && (round_cib in delays_pre_endorsement[va])) { // preendo  if (round_cib != max_round) { <= aller chercher round cib
                                let t_op_pre_valide_i = delays_pre_endorsement[va][round_cib]
                                t_op_pre_valide_i.sort(function (a, b) { return a - b }); // sort number in ascending order:
                                let position_of_delegate = 1 + t_op_pre_valide_i.indexOf(delay)
                                t_valide.push({ "type": "preendo", "bloc": va, "timestamp_delay": delay, cat: "Preendorsement round " + round_cib, "position in sample": position_of_delegate, "size of sample": t_op_pre_valide_i.length })
                            }
                            else if((va in delays_endorsement) && (round_cib in delays_endorsement[va])) {
                                let t_op_valide_i = delays_endorsement[va][round_cib]
                                t_op_valide_i.sort(function (a, b) { return a - b }); // sort number in ascending order:
                                let position_of_delegate = 1 + t_op_valide_i.indexOf(delay)
                                t_valide.push({ "type": "endo", "bloc": va, "timestamp_delay": delay, cat: "Endorsement", "position in sample": position_of_delegate, "size of sample": t_op_valide_i.length })
                            }
                        }
                    })
            }
        })
    });
    return t_valide
}

function delays_distribution_of_operations(dict_data) { //ex : tri_greeks
    let t_op_valide = {}; // t_op_erreur[k]= t_op_erreur_i
    let t_op_pre_valide = {}; // contient plusieurs dict, chacun associé à un round. Ces dict contiennent 2 listes, qui sont les délais de réception pour les Préendorsement et les endorsmenet invalides 
    Object.entries(dict_data).forEach(([va, v]) => {
        let t_op_valide_i = {}; // t_op_erreur[k]= t_op_erreur_i
        let t_op_pre_valide_i = {}; // t_op_erreur[k]= t_op_erreur_i
        let t_baker = {};
        if ("blocks" in v) {
            v["blocks"].forEach((element) => {
                let round = 0;
                if ("round" in element) round = element["round"];
                if ("timestamp" in element) t_baker[round] = new Date(element["timestamp"]);
                t_op_valide_i[round] = []; // t_op_valide[i][t_approbations]
                t_op_pre_valide_i[round] = [];//t_op_valide[i][t_pre_approbations]

            })
        }
        Object.entries(v["endorsements"]).forEach(([_k, baker_ops]) => {
            if ("operations" in baker_ops)
                baker_ops["operations"].forEach((operation) => {
                    let round_cib = 0;
                    if ("round" in operation) round_cib = operation["round"];
                    if ((round_cib in t_baker) && ("reception_time" in operation) && (operation["reception_time"] != null)) {
                        let delay = new Date(new Date(operation["reception_time"]) - t_baker[round_cib]).getSeconds();
                        if (("kind" in operation)) { // preendo  if (round_cib != max_round) { <= aller chercher round cib
                            t_op_pre_valide_i[round_cib].push(delay);
                        }
                        else {
                            t_op_valide_i[round_cib].push(delay);
                        }
                    }
                })
        })
        t_op_pre_valide[va] = t_op_pre_valide_i;
        t_op_valide[va] = t_op_valide_i;
    })
    return [t_op_pre_valide, t_op_valide]
}

function classify_operations(dict_data, delegate = "") {
    let operations_logs = {}; // COntient les différentes catégories d'opérations, ainsi qu'une liste des délégué associé aux op de chaque catégorie
    Object.entries(dict_data).forEach(([_, v]) => {
        let t_baker = {};
        if ("blocks" in v) {
            v["blocks"].forEach((element) => {
                let round = 0;
                if ("round" in element) round = element["round"];
                if ("timestamp" in element) t_baker[round] = new Date(element["timestamp"]);
                operations_logs[round] = { "approbations": { "valide": [], "manque": [], "oublie": [], "sequestre": [], "invalide": [], "inconnu": [] }, "pre_approbations": { "valide": [], "manque": [], "oublie": [], "sequestre": [], "invalide": [], "inconnu": [] } }
            })
            if ("endorsements" in v) {
                Object.entries(v["endorsements"]).forEach(([_, baker_ops]) => {
                    if (baker_ops["delegate"] == delegate || delegate == "") { // To look at operation of a specific delegate 
                        if ("operations" in baker_ops) {
                            baker_ops["operations"].forEach((operation) => {
                                let round_cib = 0;
                                if ("round" in operation) round_cib = operation["round"];
                                //Valide
                                if ((round_cib in t_baker) && ("reception_time" in operation) && (operation["reception_time"] != null)) {
                                    let delay = new Date(new Date(operation["reception_time"]) - t_baker[round_cib]).getSeconds();
                                    if (("kind" in operation)) { // preendo  if (round_cib != max_round) { <= aller chercher round cib
                                        operations_logs[round_cib]["pre_approbations"]["valide"].push(baker_ops["delegate"]);
                                    }
                                    else { // Est-ce qu'on prend pas en compte le fait que l'endo soit inclus au bloc ?? 
                                        operations_logs[round_cib]["approbations"]["valide"].push(baker_ops["delegate"]);
                                    }
                                }
                                //Si l'OP est valide, reception time existe pas / est null, mais op incluse(endo) =>  preendo/endo SEQUESTRE
                                if ((round_cib in t_baker) && (!("errors" in operation)) && ((!("reception_time" in operation)) || (operation["reception_time"] == null))) {
                                    if (("kind" in operation)) { // preendo  if (round_cib != max_round) { <= aller chercher round cib
                                        operations_logs[round_cib]["pre_approbations"]["sequestre"].push(baker_ops["delegate"]);
                                    }
                                    else if ("included_in_blocks" in operation) {
                                        operations_logs[round_cib]["approbations"]["sequestre"].push(baker_ops["delegate"]);
                                    }
                                }
                                //Si l'OP est valide, reception time, mais non inclus au bloc => endo oublié
                                if ((!("kind" in operation)) && (round_cib in t_baker) && (!("errors" in operation)) && ("reception_time" in operation) && (!("kind" in operation))) {
                                    operations_logs[round_cib]["approbations"]["oublie"].push(baker_ops["delegate"]);
                                }
                                // Comment on fait les invalides ?? Si erreur autre que : op reçu avant bloc candidat
                            });
                        }
                        else { // 0 operations => // => bloc missed by delegate
                            console.log(operations_logs)
                            operations_logs[0]["approbations"]["manque"].push(baker_ops["delegate"])
                        }
                    }
                });
            }
        }
    });
    return operations_logs
}
/*
const tri_consensus_operation_specific_address = function (adress, dict_data) {
    t_op_valide = { "timestamp": { "endo": {}, "preendo": {} }, "reception": { "endo": {}, "preendo": {} } }; // contient plusieurs dict, chacun associé à un round. Ces dict contiennent 2 listes, qui sont les délais de réception pour les Préendorsement et les endorsmenet valides 
    t_op_retard = { "timestamp": { "endo": {}, "preendo": {} }, "reception": { "endo": {}, "preendo": {} } }; // 
    missed_block = [];
    op_sequestre = [];
    op_oublies = [];
    t_baker = {};
    max_round = 0; // round final du bloc
    t_recep = {};// date de réception du bloc 

    try {
        Object.entries(dict_data).forEach(([k, v]) => {
            let va = k;
            dict_data[va]["blocks"].forEach((element) => {
                var round = 0;
                if (typeof element["round"] !== "undefined") round = element["round"]
                else round = 0;
                t_baker[round] = new Date(element["timestamp"]);
                t_recep[round] = new Date(element["reception_time"]);

            })
            Object.entries(dict_data[va]["endorsements"]).forEach(([k, v]) => {
                if (v['delegate'] == adress) { //s'il s'agit du délégué visé 
                    try { //traitement des blocs avec block round > 0 et payload round = 0                                      
                        if ("operations" in v) {
                            var participation_in_round = [];
                            for (let i = 0; i < (v["operations"]).length; i++) {

                                round_cib = v["operations"][i]["round"];
                                participation_in_round.push(round_cib);
                                if ("errors" in v["operations"][i]) { // L'OP n'est pas valide ?
                                    if ("expected_max" in v["operations"][i]["errors"][0]) {
                                        if (("kind" in v["operations"][i])) { // preendo  if (round_cib != round_bloc) { <= aller chercher round cib
                                            if (v["operations"][i]["errors"][0]["kind"] == "temporary") {



                                                if (("reception_time" in v["operations"][i]) && (v["operations"][i]["reception_time"] != null)) {

                                                    if (v["operations"][i]["errors"][0]["expected_max"] in t_op_retard["timestamp"]["preendo"]) {
                                                        t_op_retard["timestamp"]["preendo"]["expected_max"][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_baker[round_cib]).getSeconds(); // 
                                                        t_op_retard["reception"]["preendo"]["expected_max"][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_recep[round_cib]).getSeconds(); // 
                                                    }
                                                    else {
                                                        t_op_retard["timestamp"]["preendo"]["expected_max"] = {};
                                                        t_op_retard["reception"]["preendo"]["expected_max"] = {};
                                                        t_op_retard["timestamp"]["preendo"]["expected_max"][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_baker[round_cib]).getSeconds(); // 
                                                        t_op_retard["reception"]["preendo"]["expected_max"][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_recep[round_cib]).getSeconds();
                                                    }

                                                }


                                            }



                                        }
                                        else {//endo

                                            if (v["operations"][i]["errors"][0]["kind"] == "temporary") {
                                                if (("reception_time" in v["operations"][i]) && (v["operations"][i]["reception_time"] != null)) {

                                                    t_op_retard["timestamp"]["endo"][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_baker[round_cib]).getSeconds(); // 
                                                    t_op_retard["reception"]["endo"][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_recep[round_cib]).getSeconds();
                                                }
                                            }
                                        }
                                    }

                                }
                                else { //L'OP est valide ?

                                    if (("kind" in v["operations"][i])) { // preendo  if (round_cib != round_bloc) { <= aller chercher round cib
                                        if (("reception_time" in v["operations"][i]) && ("round" in v["operations"][i]) && (v["operations"][i]["reception_time"] != null)) {

                                            if (round_cib in t_op_valide["timestamp"]["preendo"]) {
                                                t_op_valide["timestamp"]["preendo"][round_cib][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_baker[round_cib]).getSeconds(); //
                                                t_op_valide["reception"]["preendo"][round_cib][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_recep[round_cib]).getSeconds(); //
                                            } else {
                                                t_op_valide["timestamp"]["preendo"][round_cib] = {};
                                                t_op_valide["reception"]["preendo"][round_cib] = {};
                                                t_op_valide["timestamp"]["preendo"][round_cib][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_baker[round_cib]).getSeconds(); //
                                                t_op_valide["reception"]["preendo"][round_cib][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_recep[round_cib]).getSeconds(); //

                                            }
                                        }// pas de "else", il y a pas moyen de savoir si un preendo est oublié
                                    }
                                    else {//endo

                                        if (("reception_time" in v["operations"][i]) && ("round" in v["operations"][i])) {
                                            if ("included_in_blocks" in v["operations"][i]) {
                                                if (v["operations"][i]["reception_time"] != null) {


                                                    t_op_valide["timestamp"]["endo"][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_baker[round_cib]).getSeconds(); //
                                                    t_op_valide["reception"]["endo"][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_recep[round_cib]).getSeconds(); //


                                                }

                                            } else {
                                                op_oublies.push(k);
                                            }
                                        }
                                        else {
                                            if (("included_in_blocks" in v["operations"][i]) && (!("reception_time" in v["operations"][i]))) {
                                                op_sequestre.push(k);
                                            }
                                        }
                                    }
                                }
                            }

                            let one = Object.keys(t_baker);
                            console.log(one, participation_in_round)
                            var round_missed = one.filter(function (item) { //obj :obtenir les rounds manqués par un délégué dans un bloc 
                                console.log(item, participation_in_round.indexOf(+item) === -1);
                                return participation_in_round.indexOf(+item) === -1;
                            });
                            if (round_missed.length > 0) {
                                missed_block.push([va, round_missed]);
                            }
                        }
                        else {
                            missed_block.push([va, [-1]]);//ajouter aux manqués !!
                        }
                        try {
                            if ("operations" in v) {
                                for (let i = 0; i < v["operations"].length; i++) {//
                                    if ((("errors" in v["operations"][i]))) { //dès lors qu'on trouve une erreur, on vérifier qu'il s'agit d'une erreur concernant le block ciblé(va) et son round.
                                        if (v["operations"][i]["errors"][0]["kind"] == "temporary") {
                                            var block_cib = (v["operations"][i]["errors"][0]["provided"]).toString();
                                            var round_cib = v["operations"][i]["round"];
                                            if (block_cib == va) {
                                                if ("kind" in v["operations"][i]) { // c'est un preendo
                                                    if (round_cib in t_op_valide["timestamp"]["preendo"]) {
                                                        t_op_valide["timestamp"]["preendo"][round_cib][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_baker[round_cib]).getSeconds(); //
                                                        t_op_valide["reception"]["preendo"][round_cib][va] = 0; //
                                                    } else {
                                                        t_op_valide["timestamp"]["preendo"][round_cib] = {};
                                                        t_op_valide["reception"]["preendo"][round_cib] = {};
                                                        t_op_valide["timestamp"]["preendo"][round_cib][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_baker[round_cib]).getSeconds(); //
                                                        t_op_valide["reception"]["preendo"][round_cib][va] = 0; //

                                                    }
                                                }
                                                else {
                                                    t_op_valide["timestamp"]["endo"][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_baker[round_cib]).getSeconds(); //
                                                    t_op_valide["reception"]["endo"][va] = new Date(new Date(v["operations"][i]["reception_time"]) - t_recep[round_cib]).getSeconds(); //
                                                }

                                            }

                                        }
                                    }

                                }
                            }

                        }
                        catch (error) {
                            console.log(error); // endo manqués
                        }
                    } catch (error) {

                        console.log(error);

                    }
                }
            });

        });
    } catch (e) { console.log(e) }
    console.log(t_op_valide, missed_block)
}*/

/*
const tri_endorsement_reception_delays_for_a_block = function (va_, dict_data) {
    //console.log(dict_data)
    let va = va_;
    let t_baker = {};
    let t_delai_bloc = {};
    let t_op_valide = {};
    let t_op_erreur = { "avance": { "t_approbations": [], "t_pre_approbations": [] } }; //  enlever
    let t_recep = {};// date de réception du bloc 
    let max_round = 0; // round final du bloc
    let ano_desc = {}; // COntient les différentes catégories d'opérations, ainsi qu'une liste des délégué associé aux op de chaque catégorie

    try {

        let json_data = dict_data[va];

        try {
            dict_data[va]["blocks"].forEach((element) => {
                var round = 0;
                if (typeof element["round"] !== "undefined") round = element["round"]
                else round = 0;
                t_baker[round] = new Date(element["timestamp"]);
                t_recep[round] = new Date(element["reception_time"]);
                t_delai_bloc[round] = (new Date(t_recep[round] - t_baker[round])).getSeconds();
                t_op_erreur[round] = { "t_approbations": [], "t_pre_approbations": [] };
                t_op_valide[round] = { "t_approbations": [], "t_pre_approbations": [] };
                ano_desc[round] = { "approbations": { "valide": [], "manque": [], "oublie": [], "sequestre": [], "invalide": [], "inconnu": [] }, "pre_approbations": { "valide": [], "manque": [], "oublie": [], "sequestre": [], "invalide": [], "inconnu": [] } }
            });
        } catch (e) {
            console.log(e);
        }

        if ((json_data["blocks"]).length == 0 || !("round" in json_data["blocks"][0])) {
            max_round = 0;
        }
        else {
            max_round = + dict_data[va]["blocks"][0]["round"];
            if (isNaN(max_round)) max_round = 0;
            console.log("Le nombre final de round du bloc est: " + max_round);
        }

        Object.entries(dict_data[va]["endorsements"]).forEach(([_k, v]) => {


            try { //traitement des blocs avec block round > 0 et payload round = 0
                //console.log((v["operations"]).length);
                if (!("operations" in v && (v["operations"]).length > 0))
                    ano_desc["0"]["approbations"]["manque"].push(v["delegate"])
                else
                    for (let i = 0; i < (v["operations"]).length; i++) {
                        round_cib = v["operations"][i]["round"];
                        if ("errors" in v["operations"][i]) { // L'OP n'est pas valide ?
                            if (("kind" in v["operations"][i])) { // preendo  if (round_cib != max_round) { <= aller chercher round cib
                                if ("reception_time" in v["operations"][i] && v["operations"][i]["reception_time"] != null) {
                                    if (("expected_max" in v["operations"][i]["errors"][0] ||
                                        "expected" in v["operations"][i]["errors"][0] && "provided" in v["operations"][i]["errors"][0]) &&
                                        v["operations"][i]["errors"][0]["kind"] == "temporary") {
                                        console.log("A");
                                        t_op_valide[round_cib]["t_pre_approbations"].push(0); // OP valable, reçu avant le block candidat
                                        ano_desc[round_cib]["pre_approbations"]["valide"].push(v["delegate"]);
                                    }
                                    t_op_erreur["avance"]["t_pre_approbations"].push(new Date(new Date(v["operations"][i]["reception_time"]) - t_baker[round_cib]).getSeconds());
                                }
                                else {
                                    //date de récep pas dispo
                                    ano_desc[round_cib]["pre_approbations"]["inconnu"].push([v["delegate"], "erreur date de récep pas dispo"])
                                }
                            }
                            else {//endo
                                if (("reception_time" in v["operations"][i])) {
                                    if (v["operations"][i]["reception_time"] != null) {
                                        if (("included_in_blocks" in v["operations"][i])) {
                                            t_op_erreur["avance"]["t_approbations"].push(new Date(new Date(v["operations"][i]["reception_time"]) - t_baker[round_cib]).getSeconds());
                                        }
                                        else {
                                            console.log("Pas d'inclusion au bloc => Approbation oubliée");
                                            ano_desc[round_cib]["approbations"]["inconnu"].push([v["delegate"], "erreur et inclus dans aucun bloc"]);
                                        }
                                    }
                                    else {
                                        //date de récep pas dispo
                                        ano_desc[round_cib]["approbations"]["inconnu"].push([v["delegate"], "erreur et date de recep null"]);
                                    }
                                }
                                else {
                                    ano_desc[round_cib]["approbations"]["inconnu"].push([v["delegate"], "erreur et pas de date de recep"]);
                                    // jamais reçu => Manquées
                                }
                            }
                        }
                        else { //L'OP est valide ?
                            if (("kind" in v["operations"][i])) { // preendo  if (round_cib != max_round) { <= aller chercher round cib
                                if (("reception_time" in v["operations"][i])) {
                                    if (v["operations"][i]["reception_time"] != null) {
                                        t_op_valide[round_cib]["t_pre_approbations"].push(new Date(new Date(v["operations"][i]["reception_time"]) - t_baker[round_cib]).getSeconds());
                                        ano_desc[round_cib]["pre_approbations"]["valide"].push(v["delegate"]);
                                    }
                                    else {
                                        //console.log("date de récep pas dispo");
                                        ano_desc[round_cib]["pre_approbations"]["sequestre"].push(v["delegate"]);
                                    }
                                } else {
                                    // jamais reçu => Manquées
                                    ano_desc[round_cib]["pre_approbations"]["sequestre"].push(v["delegate"]);
                                }

                            }
                            else {//endo
                                if (("reception_time" in v["operations"][i])) {
                                    if (v["operations"][i]["reception_time"] != null) {
                                        if (("included_in_blocks" in v["operations"][i])) {
                                            t_op_valide[round_cib]["t_approbations"].push(new Date(new Date(v["operations"][i]["reception_time"]) - t_baker[round_cib]).getSeconds());
                                            ano_desc[round_cib]["approbations"]["valide"].push(v["delegate"]);
                                        }
                                        else {
                                            //console.log("Pas d'inclusion au bloc => Approbation oubliée");
                                            ano_desc[round_cib]["approbations"]["oublie"].push(v["delegate"]);
                                        }
                                    }
                                    else {
                                        //date de récep pas dispo
                                        ano_desc[round_cib]["approbations"]["sequestre"].push(v["delegate"]);
                                    }
                                }
                                else {
                                    // jamais reçu => Manquées
                                    ano_desc[round_cib]["approbations"]["sequestre"].push(v["delegate"]);
                                }


                            }
                        }
                    }
            } catch (error) {
                console.log(error);
            }
        }
        );


        console.log(t_recep);
        console.log(t_baker);
        console.log(t_delai_bloc);
        console.log("t_op_erreur");
        console.log(t_op_erreur);
        console.log("t_valide");
        console.log(t_op_valide);
        console.log("delays_distribution_of_operations")
        let distrib_t = delays_distribution_of_operations(dict_data)
        console.log(distrib_t)
        console.log("ano_desc");
        console.log(ano_desc);
        let logs_operation = classify_operations(dict_data)
        console.log("ano_desc", ano_desc);
        console.log('logs_operation', logs_operation)
        resume_obs(logs_operation, t_baker);
        replaceDelegate(logs_operation);
        for (let i = 0; i <= max_round; i++) {
            //chart('p', [t_op_valide[i]["t_pre_approbations"], t_op_valide[i]["t_approbations"]], va, i, t_delai_bloc[i]);
            let va = va_;
            chart('p', [distrib_t[0][va][i], distrib_t[1][va][i]], va, i, t_delai_bloc[i]);
            console.log(i);
            console.log(t_delai_bloc[i]); .0
            2.5
            3.0
            3.5
        }
        // Affichage du resume

    } catch (e) {
        console.log(e)
        MessageErreur();
    }

}
*/


const percIntegration = function (threshold, t_op_pre_valide) {
    console.log(t_op_pre_valide[2412550])
    let t_cible = threshold;
    var pI_level = {};
    var d3_pI_level = [];
    Object.entries(t_op_pre_valide).forEach(([bloc, v_bloc]) => {
        pI_level[bloc] = {};
        try {
            console.log(typeof (v_bloc))
            Object.entries(v_bloc).forEach(([level, v_level]) => {
                //for (let [level, v_level] of Object.entries(v_bloc)) {
                var card_valide_tcible = 0;
                Object.entries(v_level).forEach(element => {
                    if (element <= t_cible) {
                        card_valide_tcible += 1;
                    }
                });

                if (isNaN(card_valide_tcible / v_level.length) == false) {
                    pI_level[bloc][level] = (card_valide_tcible / (v_level.length))
                    d3_pI_level.push({ bloc: bloc, level: level, pI: (card_valide_tcible / v_level.length) })
                }
            });
        } catch (e) { console.log(e) }
    });

    return d3_pI_level
}

const TimeForPercIntegration = function (threshold, t_op_pre_valide) {
    let l_ = threshold / 100;
    const qd_ = 25; // nombre de slots pour ce block; endorsing power ? 
    var t_min_ = {}; // dict pour relier le temps min pour qu'un bloc soit valide à son #bloc
    var d3_t_min_ = [];
    Object.entries(t_op_pre_valide).forEach(([bloc, v_bloc]) => {
        try {
            var last_round_ = Object.keys(v_bloc).map(Number);
            console.log(last_round_);
            if (last_round_.length != 1) {
                last_round_.sort(function (a, b) {
                    return a - b
                });
            }
            var block_fin = v_bloc[last_round_[last_round_.length - 1]]; // On se base pour ce graphe sur les rounds valides... généralement le dernier du bloc
            console.log(block_fin);
            block_fin.sort(function (a, b) {
                return a - b;
            });// trier le vecteur de délais de récep, par ordre croissant
            seuil_validation = block_fin[Math.ceil(qd_ * l_)]; // Date à laquelle on a le nombre min de preendo pour que le round soit valide 
            t_min_[bloc] = seuil_validation;
            if (isNaN(seuil_validation) == false) {
                d3_t_min_.push({ bloc: (+bloc), t: (+seuil_validation) });
            }
        }
        catch (e) {
            console.log(bloc, ": ", e)
        }
    });
    return d3_t_min_

}

const SeriesPercIntegration = function (t_cibles, t_op_pre_valide) {//Inclure endorsing power 
    var t_pI = [];
    t_cibles.forEach((t_cible) => {
        var pI_level = {}; // Pour chaque temps seuil, un dictionnaire garde la quantité de pre-endo reçu, x bloc et y round 
        var l_rounds = []; // 
        Object.entries(t_op_pre_valide).forEach(([bloc, v_bloc]) => {
            pI_level[bloc] = {};
            Object.entries(v_bloc).forEach(([level, v_level]) => {
                l_rounds.push(level);
                var card_valide_tcible = 0;
                for (const element of v_level) {
                    if (element <= t_cible) {
                        card_valide_tcible += 1;
                    }
                }
                if (isNaN(card_valide_tcible / v_level.length) == false) {
                    pI_level[bloc][level] = (card_valide_tcible / (v_level.length))
                }
            })
        });

        l_rounds = l_rounds.filter(onlyUnique);
        l_rounds.forEach((r) => {
            var pi_l = [];
            Object.entries(pI_level).forEach(([_, v_level]) => {
                if (r in v_level) {
                    pi_l.push(v_level[r]);
                }
            });
            t_pI.push({ temps: +t_cible, round: r, value: (pi_l.reduce((a, b) => a + b, 0) / pi_l.length) });
        });
    })
    return t_pI
}




function chart(dom, data, niveau, round, delai_recep_bloc_) {
    if (!(isEmpty(data)) && (!([[undefined, undefined]].includesArray(data)))) {
        var margin = ({ top: 25, right: 30, bottom: 30, left: 40 }),
            width = 1000, // outer width of chart, in pixels
            height = 400; // outer height of chart, in pixels 
        if ((round == 0) && (typeof (document.querySelector("p")) != 'undefined' && document.querySelector("p") != null)) { // SI on passe au round suivant, alors on supprime les graphs du niveau dernièrement observé 
            console.log(niveau + " supprimer si round 0 !!!");
            const e = document.querySelector("p");
            while (e.firstChild) {
                e.removeChild(e.lastChild);
                console.log(document.querySelector("p").childNodes);
            }

        }
        const svg = d3.select("body").select(dom).append("svg").attr("height", height).attr("viewBox", [0, 0, width, height]);

        const xAxis = svg.append("g").attr("transform", `translate(0,${height - margin.bottom})`);
        const yAxis = svg.append("g").attr("transform", `translate(${margin.left},0)`);//.append("title").text("↑ # Délégués");
        const graph = svg.append("g").attr("fill", "steelblue");

        let minValue = Object.values(data[0])[0]; //On calcule le Range des valeurs de temps mesuré pour un bloc, afin d'ajuster les dimensions d'affichage 

        let maxValue = Object.values(data[0])[0]; //On calcule le Range des valeurs de temps mesuré pour un bloc, afin d'ajuster les dimensions d'affichage 

        for (const [key, value] of Object.entries(data[0])) {
            if (value > maxValue) {
                maxValue = value;
            }
            if (value < minValue) {
                minValue = value;
            }
        }

        bins = d3.bin().thresholds(maxValue - minValue)(data[0])

        for (const [key, value] of Object.entries(data[1])) {
            if (value > maxValue) {
                maxValue = value;
            }
            if (value < minValue) {
                minValue = value;
            }
        }

        bins2 = d3.bin().thresholds(maxValue - minValue)(data[1]);
        bins3 = d3.bin().thresholds(maxValue - minValue)([delai_recep_bloc_]);
        console.log(bins3);

        bins_tot = d3.bin().thresholds(maxValue - minValue)((data[1].concat(data[0])).concat([delai_recep_bloc_])); // aide à définir x: Mauavise idéé 

        x = d3.scaleLinear()
            .domain([0, bins_tot[bins_tot.length - 1].x1])
            .range([margin.left, width - margin.right]);

        y = d3.scaleLinear()
            .domain([0, d3.max(bins_tot, d => d.length)]).nice()
            .range([height - margin.bottom, margin.top])

        graph.selectAll("rect")
            .data(bins)
            .join("rect")
            .attr("x", d => x(d.x0) + 1)
            .attr("width", 15)
            .attr("y", d => y(d.length))
            .attr("height", d => y(0) - y(d.length))
            .style("fill", "#69b3a2")
            .style("opacity", 0.6);

        graph.selectAll("rect2")
            .data(bins2)
            .join("rect")
            .attr("x", d => x(d.x0) + 1)
            .attr("width", 15)
            .attr("y", d => y(d.length))
            .attr("height", d => y(0) - y(d.length))
            .style("fill", "#404080")
            .style("opacity", 0.6);

        graph.selectAll("rect3")//block
            .data(bins3)
            .join("rect")
            .attr("x", d => x(d.x0) + 1)
            .attr("width", d => Math.max(2, x(d.x1) - x(d.x0) - 1))
            .attr("y", margin.top)
            .attr("height", height - margin.bottom - margin.top)
            .style("fill", "rgba(198, 0, 0, 1)")
            .style("opacity", 0.6);

        svg.append("text")
            .attr("x", (width / 2))
            .attr("y", 15)
            .attr("text-anchor", "middle")
            .style("font-size", "15px")
            .style("text-decoration", "underline")
            .text("Histogram of block # " + niveau + " reception delays, for round = " + round);

        svg.append("text")
            .attr("x", -margin.left + 40)
            .attr("y", 15)
            .attr("fill", "currentColor")
            .attr("text-anchor", "start")
            .text("↑ # Delegates");

        svg.append("text")
            .attr("x", width)
            .attr("y", height)
            .attr("fill", "currentColor")
            .attr("text-anchor", "end")
            .text("reception times (seconds) →");

        // Handmade legend
        svg.append("circle").attr("cx", width - 130).attr("cy", 30).attr("r", 6).style("fill", "#69b3a2")
        svg.append("circle").attr("cx", width - 130).attr("cy", 60).attr("r", 6).style("fill", "#404080")
        svg.append("text").attr("x", width - 110).attr("y", 30).text(" Preendorsements").style("font-size", "15px").attr("alignment-baseline", "middle")
        svg.append("text").attr("x", width - 110).attr("y", 60).text("Endorsements").style("font-size", "15px").attr("alignment-baseline", "middle")
        svg.append("circle").attr("cx", width - 130).attr("cy", 90).attr("r", 6).style("fill", "rgba(198, 0, 0, 1)")
        svg.append("text").attr("x", width - 110).attr("y", 90).text("Candidate block").style("font-size", "15px").attr("alignment-baseline", "middle")

        xAxis.call(d3.axisBottom(x).tickSizeOuter(2));
        yAxis.call(d3.axisLeft(y)).call(g => g.select(".domain").remove());
    }
}

const resume_obs = function (data, t_baker) {
        console.log(data);
        Object.entries(data).forEach(([level, el]) => {
            Object.entries(el).forEach(([k1, v1]) => {
                Object.entries(v1).forEach(([k2, v2]) => {
                    var new_v2 = [];
                    for (const element of v2) {
                        var adress_comp = element.slice(0, 7) + "..." + element.slice(-6, -1) + ", ";
                        new_v2.push(element + " ");
                    }
                    data[level][k1][k2] = new_v2;
                })

            })
        });
        var body = document.getElementById("resume");
        var titre_ = document.createElement('h2');
        titre_.appendChild(document.createTextNode("summary of operations received"));
        try {
            while (document.getElementById("resume").querySelector("h2")) {
                const elements2 = document.getElementById("resume").querySelector("h2")
                elements2.parentElement.removeChild(elements2);

            }
            const elements = document.getElementById("resume").querySelector("table");
            elements.parentElement.removeChild(elements);
        } catch (e) {
            //console.log(e)
        }
        var tbl = document.createElement('table');
        tbl.style.width = '100%';
        tbl.setAttribute('border', '1');
        var tbdy = document.createElement('tbody');

        let manque = ["Manquées", (data["0"]["approbations"]["manque"]).length, data["0"]["approbations"]["manque"]];// à revoir : le fait que j'introduit "1" n'est pas bon 
        let entete = ["Type", "Proportion", "Addresses of corresponding delegates"];

        var tr_manque = document.createElement('tr');
        var tr_entete = document.createElement('tr');

        entete.forEach((element) => {
            var th = document.createElement('th');
            th.appendChild(document.createTextNode(element));
            tr_entete.appendChild(th);
        })
        tbdy.appendChild(tr_entete);

        manque.forEach((element) => {
            var td = document.createElement('td');
            td.appendChild(document.createTextNode(element));
            tr_manque.appendChild(td);
        })
        tbdy.appendChild(tr_manque);

        Object.entries(data).forEach(([k, v]) => {
            if (k in t_baker) {
                var tr_round = document.createElement('tr');
                tr_round.appendChild(document.createTextNode("round:" + k));
                tbdy.appendChild(tr_round);

                let valide = ["valides", (v["approbations"]["valide"]).length];//, v["approbations"]["valide"]];//.slice(0,10)]; // à revoir 
                var tr_valide = document.createElement('tr');
                valide.forEach((element) => {
                    var td = document.createElement('td');

                    td.appendChild(document.createTextNode(element));
                    tr_valide.appendChild(td);
                })
                tbdy.appendChild(tr_valide);

                let oublie = ["Oubliées", (v["approbations"]["oublie"]).length, v["approbations"]["oublie"]]; // à revoir 
                var tr_oublie = document.createElement('tr');
                oublie.forEach((element) => {
                    var td = document.createElement('td');
                    td.appendChild(document.createTextNode(element));
                    tr_oublie.appendChild(td);
                })
                tbdy.appendChild(tr_oublie);

                let invalide = ["Invalides", (v["approbations"]["invalide"]).length, v["approbations"]["invalide"]]; // à revoir 
                var tr_invalide = document.createElement('tr');
                invalide.forEach((element) => {
                    var td = document.createElement('td');
                    td.appendChild(document.createTextNode(element));
                    tr_invalide.appendChild(td);
                })
                tbdy.appendChild(tr_invalide);

                let inconnu = ["Inconnu", (v["approbations"]["inconnu"]).length, v["approbations"]["inconnu"]]; // à revoir 
                var tr_inconnu = document.createElement('tr');
                inconnu.forEach((element) => {
                    var td = document.createElement('td');
                    td.appendChild(document.createTextNode(element));
                    tr_inconnu.appendChild(td);
                })
            }
        });

        tbl.appendChild(tbdy);
        body.appendChild(titre_);
        body.appendChild(tbl);
    
}

function chart_gamma(data) {
    if (!(isEmpty(data))) {
    var margin = ({ top: 40, right: 180, bottom: 30, left: 40 }),
        width = 850 - margin.left - margin.right;//1000, // outer width of chart, in pixels
    height = 500 - margin.top - margin.bottom;//400; // outer height of chart, in pixels 
    try {
        try {
            while (document.getElementById("gamma").querySelector("svg")) {
                const elements3 = document.getElementById("gamma").querySelector("svg");
                document.getElementById("gamma").removeChild(elements3);
            }
        } catch (e) {
            console.log(e)
        }
        const svg = d3.select("body").select("#gamma").append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom);

        const svgg = svg.append("g")
            .attr("transform",
                "translate(" + margin.left + "," + margin.top + ")");

        var sumstat = d3.group(data, d => d.round);

        // Add X axis --> it is a date format
        var x = d3.scaleLinear()
            .domain(d3.extent(data, d => d.temps))
            .range([0, width]);
        svgg.append("g")
            .attr("transform", "translate(0," + height + ")")
            .call(d3.axisBottom(x).ticks(5));

        // Add Y axis
        var y = d3.scaleLinear()
            .domain([0, 100])
            .range([height, 0]);
        svgg.append("g")
            .attr("transform", "translate(0,0)")
            .call(d3.axisLeft(y).tickSizeOuter(0));

        // color palette
        var res = Array.from(sumstat.keys()); // list of group names
        var color = d3.scaleOrdinal()
            .domain(res)
            .range(['#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', '#ffff33', '#a65628', '#f781bf', '#999999'])

        var legend = d3.legendColor().scale(color);

        svgg.append("g").attr("transform", "translate(" + (width) + ",10)").call(legend);

        svgg.selectAll(".line")
            .data(sumstat)
            .join("path")
            .attr('fill', 'none')
            .attr('stroke-width', 1.5)
            .attr('stroke', d => color(d[0]))
            .attr("d", d => {
                return d3.line()
                    .x(d => x(d.temps))
                    .y(d => y(100 * d.value))
                    (d[1])
            });

        svgg.append("text")
            .attr("x", -margin.left + 40)
            .attr("y", -5)
            .attr("fill", "currentColor")
            .attr("text-anchor", "start")
            .text("↑ % Preendorsements received before threshold time");

        svgg.append("text")
            .attr("x", width + margin.right)
            .attr("y", height)
            .attr("fill", "currentColor")
            .attr("text-anchor", "end")
            .text(" threshold time (seconds) →");

        svgg.append("text")
            .attr("x", width + 40)
            .attr("y", 0)
            .attr("fill", "currentColor")
            .attr("text-anchor", "end")
            .text("Level:");


    } catch (e) { console.log(e) }
    }else{
        alert("Invalid value. Enter valid threshold range and/or valid block level range.")
    }
}

const replaceDelegate = function (data) {
    if (Object.keys(data).length <= 1) {

        try {
            const elements2 = document.getElementById("tab_delegates").querySelector("h2")
            elements2.parentElement.removeChild(elements2);
        } catch (e) {
            console.log(e)
        }
        try {
            const elements = document.getElementById("tab_delegates").querySelector("table");
            elements.parentElement.removeChild(elements);
        } catch (e) {
            console.log(e)
        }

    } else {

        new Promise((resolve, reject) => {
            var replaceDelegate_ = {};
            const lgth = Object.keys(data).length;
            console.log(data);

            for (let i = 1; i < lgth; i++) {
                replaceDelegate_[i] = { "entrants": [], "sortants": [] };
                console.log(replaceDelegate_);
            }

            for (let i = 1; i < lgth; i++) {
                console.log(data[i]);
                let delegate_next = data[i]["pre_approbations"]["valide"];
                let delegate_prec = data[i - 1]["pre_approbations"]["valide"];
                for (let x of delegate_prec) {
                    if (delegate_next.includes(x) == false) {
                        replaceDelegate_[i]["sortants"].push(x);
                    }
                }
                for (let x of delegate_next) {
                    if (delegate_prec.includes(x) == false) {
                        replaceDelegate_[i]["entrants"].push(x);
                    }
                }
            }

            resolve(replaceDelegate_)
        }).then(resolve => {
            console.log(resolve);

            console.log("replaceDelegate");
            var body = document.getElementById("tab_delegates");

            try {
                const elements2 = document.getElementById("tab_delegates").querySelector("h2")
                elements2.parentElement.removeChild(elements2);
            } catch (e) {
                //console.log(e)
            }
            try {
                const elements = document.getElementById("tab_delegates").querySelector("table");
                elements.parentElement.removeChild(elements);
            } catch (e) {
                //console.log(e)
            }

            var titre_2 = document.createElement('h2');
            titre_2.appendChild(document.createTextNode("summary of incoming/outgoing delegates in each round"));
            body.appendChild(titre_2);

            var tbl = document.createElement('table');
            tbl.style.width = '100%';
            tbl.setAttribute('border', '1');
            var tbdy = document.createElement('tbody');

            var tr_entete = document.createElement('tr');
            let entete = ["# of incoming delegates", "adresses of incoming delegates", "# of outgoing delegates", "adresses of outgoing delegates"];

            entete.forEach((element) => {
                var th = document.createElement('th');
                th.appendChild(document.createTextNode(element));
                tr_entete.appendChild(th);
            })
            tbdy.appendChild(tr_entete);

            Object.entries(resolve).forEach(([k, v]) => {
                var tr_round = document.createElement('tr');
                tr_round.appendChild(document.createTextNode("round:" + k));
                tbdy.appendChild(tr_round);

                var tr_deleg = document.createElement('tr');
                let deleg_ = [v["entrants"].length, v["entrants"], v["sortants"].length, v["sortants"]];
                deleg_.forEach((element) => {
                    var td = document.createElement('td');
                    td.appendChild(document.createTextNode(element));
                    tr_deleg.appendChild(td);
                })
                tbdy.appendChild(tr_deleg);
            });
            tbl.appendChild(tbdy);
            body.appendChild(tbl);
        })
    }
}

function chart_alpha(data) {
    if (!(isEmpty(data))) {
    var margin = ({ top: 40, right: 60, bottom: 30, left: 40 }),
        width = 560 - margin.left - margin.right;//1000, // outer width of chart, in pixels
    height = 400 - margin.top - margin.bottom;//400; // outer height of chart, in pixels 
    try {
        try {
            while (document.getElementById("alpha").querySelector("svg")) {
                const elements3 = document.getElementById("alpha").querySelector("svg");
                document.getElementById("alpha").removeChild(elements3);
            }
        } catch (e) {
            console.log(e)
        }
        const svg = d3.select("body").select("#alpha").append("svg").attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
            .append("g")
            .attr("transform",
                "translate(" + margin.left + "," + margin.top + ")");

        var x = d3.scaleLinear()
            .domain(d3.extent(data, function (d) { return d.bloc; }))
            .range([0, width]);
        svg.append("g")
            .attr("transform", "translate(0," + (height + 5) + ")")
            .call(d3.axisBottom(x).ticks(5).tickSizeOuter(0));


        var y = d3.scaleLinear()
            .domain(d3.extent(data, function (d) { return 100 * d.pI; }))
            .range([height, 5]);
        svg.append("g")
            .attr("transform", "translate(-5,0)")
            .call(d3.axisLeft(y).tickSizeOuter(0));

        var colors = d3.scaleLinear()
            .domain(d3.ticks(0, 4, 6))
            .range(["#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598",
                "#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142"]);

        var legend = d3.legendColor().scale(colors);

        svg.append("g").attr("transform", "translate(487,10)").call(legend);

        // Add the line
        svg.selectAll("circle")
            .data(data)
            .enter()
            .append("circle")
            .attr("fill", d => colors(d.level))
            .attr("stroke", "none")
            .attr("cx", function (d) { return x(d.bloc) })
            .attr("cy", function (d) { return y(100 * d.pI) })
            .attr("r", 2);

        svg.append("text")
            .attr("x", -margin.left + 40)
            .attr("y", 0)
            .attr("fill", "currentColor")
            .attr("text-anchor", "start")
            .text("↑ % Preendorsements received before threshold time");

        svg.append("text")
            .attr("x", width + margin.right)
            .attr("y", height)
            .attr("fill", "currentColor")
            .attr("text-anchor", "end")
            .text(" # bloc →");

        svg.append("text")
            .attr("x", width + margin.right)
            .attr("y", 0)
            .attr("fill", "currentColor")
            .attr("text-anchor", "end")
            .text("Level:");



    } catch (e) { console.log(e) }
}else{
    alert("Invalid value. Enter valid threshold and/or valid block level range.")
}

}

function chart_beta(data) {
    if (!(isEmpty(data))) {
    var margin = ({ top: 40, right: 60, bottom: 30, left: 40 }),
        width = 800 - margin.left - margin.right;//1000, // outer width of chart, in pixels
    height = 400 - margin.top - margin.bottom;//400; // outer height of chart, in pixels 
    try {
        try {
            while (document.getElementById("beta").querySelector("svg")) {
                const elements3 = document.getElementById("beta").querySelector("svg");
                document.getElementById("beta").removeChild(elements3);
            }
        } catch (e) {
            console.log(e)
        }
        var svg = d3.select("body").select("#beta").append("svg").attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
            .append("g")
            .attr("transform",
                "translate(" + margin.left + "," + margin.top + ")");

        var x = d3.scaleLinear()
            .domain(d3.extent(data, function (d) { return d.bloc; }))
            .range([0, width]);
        svg.append("g")
            .attr("transform", "translate(0," + (height + 5) + ")")
            .call(d3.axisBottom(x).ticks(5).tickSizeOuter(0)); //(0," + (height + 5) + ")")


        var y = d3.scaleLinear()
            .domain(d3.extent(data, function (d) { return d.t; }))
            .range([height, 0]);
        svg.append("g")
            .attr("transform", "translate(-5,0)")
            .call(d3.axisLeft(y).tickSizeOuter(0));

        // Add the line
        svg.append("path")
            .datum(data)
            .attr("fill", "none")
            .attr("stroke", "#69b3a2")
            .attr("stroke-width", 2)
            .attr("d", d3.line()
                .x(function (d) { return x(d.bloc) })
                .y(function (d) { return y(d.t) })
            )

        // Add the line
        svg.selectAll("myCircles")
            .data(data)
            .enter()
            .append("circle")
            .attr("fill", "#3288BD")
            .attr("stroke", "none")
            .attr("cx", function (d) { return x(d.bloc) })
            .attr("cy", function (d) { return y(d.t) })
            .attr("r", 2);

        svg.append("text")
            .attr("x", -margin.left + 40)
            .attr("y", 0)
            .attr("fill", "currentColor")
            .attr("text-anchor", "start")
            .text("↑ Temps minimun(s)");

        svg.append("text")
            .attr("x", width + margin.right)
            .attr("y", height)
            .attr("fill", "currentColor")
            .attr("text-anchor", "end")
            .text(" # bloc →");

    } catch (e) { console.log(e) }
    }else{
        alert("erreur")
    }
}

function chart_consensus_operation_specific_address(data) {
    if (!(isEmpty(data))) {
        //console.log(data)
        var margin = ({ top: 40, right: 220, bottom: 30, left: 40 }),
            width = 870 - margin.left - margin.right;//1000, // outer width of chart, in pixels
        height = 500 - margin.top - margin.bottom;//400; // outer height of chart, in pixels 
        try {
            try {
                while (document.getElementById("dataviz").querySelector("svg")) {
                    const elements3 = document.getElementById("dataviz").querySelector("svg");
                    document.getElementById("dataviz").removeChild(elements3);
                }
            } catch (e) {
                console.log(e)
            }
            const svg = d3.select("body").select("#dataviz").append("svg")
                .attr("width", width + margin.left + margin.right)
                .attr("height", height + margin.top + margin.bottom);

            const svgg = svg.append("g")
                .attr("transform",
                    "translate(" + margin.left + "," + margin.top + ")");

            var myX = d3.map(data, d => d.bloc)
            var myY = d3.map(data, d => d.timestamp_delay)
            //console.log("my", myX, myY);

            var sumstat = d3.group(data, d => d.cat);
            //console.log(sumstat)

            // Add X axis --> it is a date format
            var x = d3.scaleLinear()
                .domain(d3.extent(data, d => d.bloc))
                .range([0, width]);
            svgg.append("g")
                .attr("transform", "translate(0," + height + ")")
                .call(d3.axisBottom(x).ticks(5));

            // Add Y axis
            var y = d3.scaleLinear()
                .domain(d3.extent(data, d => d.timestamp_delay))
                .range([height, 0]);
            svgg.append("g")
                .attr("transform", "translate(0,0)")
                .call(d3.axisLeft(y).tickSizeOuter(0));

            // color palette
            var res = Array.from(sumstat.keys()); // list of group names
            var color = d3.scaleOrdinal()
                .domain(res)
                .range(['#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', '#ffff33', '#a65628', '#f781bf', '#999999'])

            var legend = d3.legendColor().scale(color);

            svgg.append("g").attr("transform", "translate(" + (width) + ",10)").call(legend);

            //MAJ
            var color_dots = d3.scaleOrdinal()
                .domain(["Endorsement", "Preendorsement round 0", "Preendorsement round 1", "Preendorsement round 2", "Preendorsement round 3", "Preendorsement round 4", "Preendorsement round 5", "Preendorsement round 6", "Preendorsement round 7"])
                .range(['#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', '#ffff33', '#a65628', '#f781bf', '#999999']);
            //ar legend = d3.legendColor().scale(color_dots);



            svgg.append('g')
                .selectAll("dot")
                .data(data, function (d) { return d.bloc + ':' + d.timestamp_delay; })
                .enter()
                .append("circle")
                .attr("fill", "#000000")
                .attr("stroke", "none")
                .attr('stroke-width', 1.5)
                .attr("cx", function (d) { return x(d.bloc) })
                .attr("cy", function (d) { return y(d.timestamp_delay) })
                .attr("r", 3)
                .style("fill", function (d) { return color(d.cat) })
                .on("mouseover", function (event, d) {// pour afficher les infos d'un point : https://stackoverflow.com/questions/67473725/how-to-fix-undefined-issue-on-d3-tooltip
                    var xPosition = parseFloat(d3.select(this).attr("cx"));
                    var yPosition = parseFloat(d3.select(this).attr("cy"));


                    svg.append("text")
                        .attr("id", "tooltip")
                        .attr("x", xPosition)
                        .attr("y", yPosition)
                        .text("Bloc: " + d.bloc)

                })
                .on("mouseout", function () {

                    //Remove the tooltip
                    d3.select("#tooltip").remove();
                }
                )

            svgg.append("text")
                .attr("x", -margin.left + 40)
                .attr("y", -5)
                .attr("fill", "currentColor")
                .attr("text-anchor", "start")
                .text("↑ Delays between operations receipt time and candidate block timestamps (seconds)");

            svgg.append("text")
                .attr("x", width + margin.right - 120)
                .attr("y", height)
                .attr("fill", "currentColor")
                .attr("text-anchor", "end")
                .text(" # Bloc →");

            svgg.append("text")
                .attr("x", width + 40)
                .attr("y", 0)
                .attr("fill", "currentColor")
                .attr("text-anchor", "end")
                .text("Type:");

        } catch (e) { console.log(e) }
    }
    else{
            alert("No result. \nIt can be due to:\n - No slot for this address during this period \n -Invalid address or range of bloc ")
    }
}

function isEmpty(obj) {
    return Object.keys(obj).length === 0;
}

Array.prototype.includesArray = function (arr) {
    return this.map(i => JSON.stringify(i)).includes(JSON.stringify(arr))
}
