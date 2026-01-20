import json
with open("data/A_people.json", encoding='utf-8') as file:
        data = json.load(file)

politicians=[]

for dictionary in data:
        if "ontology/party_label" in dictionary:
                if dictionary["ontology/party_label"]=="Democratic Party (United States)" or dictionary["ontology/party_label"]=="Republican Party (United States)":
                        politicians.append(dictionary)

politician_data=[]
politician_birthdate_lists=[]
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
                        # birthyear=int(birthyear_string)
                elif type(politician["ontology/birthDate"]) is list:
                        birthyear=None
        else:
                birthyear=None

        politician_data.append({"name":politician["title"], "party":politician["ontology/party_label"], "n_spouses":n_spouses, "n_children":n_children, "religion":religion, "birthyear":birthyear})
#creates a list called "politician_data" that contains dictionairies for each wikipedia page of a politician, their political party, and their number of spouses
for dictionary in politician_data:
        with open("politicians_data_A.csv", "a", encoding="utf-8") as file:
                file.write(f"{dictionary} \n")