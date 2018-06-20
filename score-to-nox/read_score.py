from datetime import datetime
with open('..\csv-to-score\output') as input:
    begin, end = input.readline().strip().split(',')
    print(datetime.strptime(begin, '%Y-%m-%d %H:%M:%S.%f'))
    print(datetime.strptime(end, '%Y-%m-%d %H:%M:%S.%f'))
