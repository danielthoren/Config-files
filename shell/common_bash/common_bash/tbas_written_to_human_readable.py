import math

tbasWritten = int(input("Enter TBA:s written: "))
sectorSize = int(input("Enter sector size (default 512): ") or "512")

gbWritten = (tbasWritten * sectorSize) / math.pow(1024, 3)
tbWritten = (tbasWritten * sectorSize) / math.pow(1024, 4)

print("GB written: {:.0f}".format(gbWritten))
print("TB written: {:.2f}".format(tbWritten))
