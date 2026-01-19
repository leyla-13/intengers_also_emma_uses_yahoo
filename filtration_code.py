import json
with open("data/A_people.json", encoding='utf-8') as file:
        data = json.load(file)

politicians=[]

for dictionary in data:
        if "ontology/party_label" in dictionary:
                if dictionary["ontology/party_label"]=="Democratic Party (United States)" or dictionary["ontology/party_label"]=="Republican Party (United States)":
                        politicians.append(dictionary)

politician_data=[]

for politician in politicians:
        if "ontology/spouse_label" in politician:
                politician_data.append({"name":[politician["title"]], "party":[politician["ontology/party_label"]], "n_spouses":[(len(politician["ontology/spouse_label"]))]})
#creates a list called "politician_data" that contains dictionairies for each wikipedia page of a politician, their political party, and their number of spouses
print(politician_data)