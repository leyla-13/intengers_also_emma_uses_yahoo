import json

datasets=["A_people", "B_people", "C_people", "D_people", "E_people", "F_people", "G_people", "H_people", "I_people", "J_people", "K_people", "L_people", "M_people", "N_people", "O_people", "P_people", "Q_people", "R_people", "S_people", "T_people", "U_people", "V_people", "W_people", "X_people", "Y_people", "Z_people"]
for dataset in datasets:
        with open(f"data/{dataset}.json", encoding='utf-8') as file:
                data = json.load(file)
        politicians=[]
        for dictionary in data:
                if "ontology/party_label" in dictionary:
                        if dictionary["ontology/party_label"]=="Democratic Party (United States)" or dictionary["ontology/party_label"]=="Republican Party (United States)":
                                politicians.append(dictionary)
        politician_data=[]
        for politician in politicians:
                if "ontology/spouse_label" in politician:
                        n_spouses=1
                        if type(politician["ontology/spouse_label"]) is list:
                                n_spouses=len(politician["ontology/spouse_label"])
                else:
                        n_spouses=0
                if "ontology/child_label" in politician:
                        if type(politician["ontology/child_label"]) is str:
                                n_children=1
                        elif type(politician["ontology/child_label"]) is list:
                                n_children=len(politician["ontology/child_label"])
                else:
                        n_children=0
                if "ontology/religion_label" in politician:
                        religion=politician["ontology/religion_label"]
                else:
                        religion=None
                if "ontology/birthDate" in politician:
                        if type(politician["ontology/birthDate"]) is str:
                                birthdate=politician["ontology/birthDate"]
                                birthdate_split=birthdate.split("-")
                                birthyear=int(birthdate_split[0])
                        elif type(politician["ontology/birthDate"]) is list:
                                birthyear=None
                else:
                        birthyear=None

                politician_data.append({"name":politician["title"], "party":politician["ontology/party_label"], "n_spouses":n_spouses, "n_children":n_children, "religion":religion, "birthyear":birthyear})
        #creates a list called "politician_data" that contains dictionairies for each wikipedia page of a politician, their political party, and their number of spouses
        for dictionary in politician_data:
                with open("politicians_data_all.csv", "a", encoding="utf-8") as file:
                        file.write(f"{dictionary} \n")