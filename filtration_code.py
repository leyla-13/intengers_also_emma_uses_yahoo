import json
with open("data/A_people.json", encoding='utf-8') as file:
        data = json.load(file)

politicians=[]

for dictionairy in data:
        if "ontology/party_label" in dictionairy:
                if dictionairy["ontology/party_label"]=="Democratic Party (United States)" or dictionairy["ontology/party_label"]=="Republican Party (United States)":
                        politicians.append(dictionairy)

print(politicians)