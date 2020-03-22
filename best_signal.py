def best_signal(measurement_name: str) -> str:
    number = int(measurement_name[-2:])
    if number in (6,12,18,28): # What about 8?
        return 'PES 2';
    elif number == 5:
        return 'PES 4';
    return 'PES 3';
