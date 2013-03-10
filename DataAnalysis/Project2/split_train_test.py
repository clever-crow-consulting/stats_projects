import sys

infile = open("./data/samsungData.csv")
trainfile = open("./data/samsungData_train.csv","w")
testfile = open("./data/samsungData_test.csv","w")

cnt = 0
for aline in infile:
    if cnt == 0:
        trainfile.write(aline)
        testfile.write(aline)
    cnt += 1
    if cnt < 2: continue
    parts = aline.split(",")
    if int(parts[-2]) in (1,3,5,6):
            trainfile.write(aline)
    if int(parts[-2]) in (27,28,29,30):
            testfile.write(aline)
    
    