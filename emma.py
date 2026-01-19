all_people = ["A_people", "B_people", "C_people", "D_people", "E_people", "F_people", "G_people", "H_people", "I_people", "J_people", "K_people", "L_people", "M_people", "N_people", "O_people", "P_people", "Q_people", "R_people", "S_people", "T_people", "U_people", "V_people", "W_people", "X_people", "Y_people", "Z_people"]

for x in all_people:
    with open(f"{x}.json", encoding='utf-8') as file:
        contents = file.read()

