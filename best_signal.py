def best_signal(measurement_name: str) -> str:
    number = int(measurement_name[-2:])
    if number in (6,12,18,28): # What about 8?
        return 'PES 2';
    elif number == 5:
        return 'PES 4';
    return 'PES 3';

def best_scoring_group(recording: Recording, measurement_name:str="Unknown recording", log:bool=False) -> None:
        scoring_names = [name.lower() for name in recording.get_all_scoring_names()];
        scoring_name = 'PSG-Marta' if 'psg-marta' in scoring_names else scoring_names[0];
        scoring_name = 'PSGPes' if 'psgpes' in scoring_names else scoring_name;
        scoring_name = 'Sigga' if 'sigga' in scoring_names else scoring_name;
        scoring_name = 'Sigga PESrescore' if 'sigga pesrescore' in scoring_names else scoring_name;
        scoring_name = 'SiggaPesKao' if 'siggapeskao' in  scoring_names else scoring_name;
        if log:
            stderr.write("{} scored based on: {}\n".format(measurement_name, scoring_name));
        recording.set_active_scoring_group(scoring_name, case_sensitive=False);
