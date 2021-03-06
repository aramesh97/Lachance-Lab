{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 24,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "1233014\n"
     ]
    }
   ],
   "source": [
    "#read in vcf\n",
    "file1open = open(\"new225.vcf\", \"r\")\n",
    "\n",
    "vcf=[]\n",
    "for line in file1open:\n",
    "    if line.startswith(\"##\"): \n",
    "        continue\n",
    "    if line.startswith(\"#CHROM\"):\n",
    "        individuals = line.strip().split(\"\\t\")\n",
    "    row = line.strip().split(\"\\t\")\n",
    "    vcf.append(row)\n",
    "\n",
    "print(len(vcf))"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 20,
   "metadata": {},
   "outputs": [],
   "source": [
    "#read in risk allele file\n",
    "\n",
    "riskallelefile = open(\"PGS000014.txt\", \"r\")\n",
    "\n",
    "\n",
    "riskallele=[]\n",
    "for line in riskallelefile:\n",
    "    if line.startswith(\"#\"):\n",
    "        continue\n",
    "    else:\n",
    "        row = line.strip().split(\"\\t\")\n",
    "        riskallele.append(row)\n",
    "\n",
    "commonsnps=[]   \n",
    "for i in vcf:\n",
    "    for j in riskallele:\n",
    "        if [i[0] and i[1]] == [j[0] and j[1]]:\n",
    "            if i not in commonsnps: \n",
    "                commonsnps.append(i)\n",
    "                break\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 27,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "----\n",
      "Falkenstein 54\n",
      "Iboussieres25-1 59\n",
      "Iboussieres31-2 60\n",
      "Rochedane 53\n",
      "BerryAuBac 57\n",
      "ANI152 62\n",
      "ANI153 63\n",
      "ANI160 63\n",
      "ANI163 39\n",
      "Bul10 63\n",
      "Bul4 39\n",
      "Bul6 42\n",
      "Bul8 62\n",
      "ANI159-ANI181 37\n",
      "I2405 59\n",
      "I2427 24\n",
      "I2431 22\n",
      "I2433 36\n",
      "I2434 54\n",
      "I2435 62\n",
      "I2440 46\n",
      "I2441 30\n",
      "I0785 58\n",
      "I0781 24\n",
      "I0633 25\n",
      "I0634 13\n",
      "I1131 19\n",
      "I1298 54\n",
      "I0676 9\n",
      "I1736 33\n",
      "I1734 16\n",
      "I1738 18\n",
      "I1732 20\n",
      "I1917 29\n",
      "I1926 56\n",
      "I2105 15\n",
      "I2110 55\n",
      "I2111 59\n",
      "I2158 57\n",
      "I2163 17\n",
      "I2165 20\n",
      "I2407 63\n",
      "I3499 38\n",
      "I3141 60\n",
      "I3151 58\n",
      "I3313 18\n",
      "I3433 22\n",
      "I3498 29\n",
      "I3879 56\n",
      "I3947 20\n",
      "I3948 18\n",
      "I2403 47\n",
      "I3714 55\n",
      "I3715 35\n",
      "I3717 35\n",
      "I3719 55\n",
      "I4088 17\n",
      "I4089 20\n",
      "I4110 30\n",
      "I4111 22\n",
      "I4112 60\n",
      "I4114 33\n",
      "I4167 60\n",
      "I4168 55\n",
      "I4175 62\n",
      "I1763 13\n",
      "I1819 10\n",
      "I2509 16\n",
      "I2510 19\n",
      "I2519 23\n",
      "I2520 18\n",
      "I2532 28\n",
      "I2533 23\n",
      "I2534 41\n",
      "I3712 59\n",
      "I3713 55\n",
      "I3716 55\n",
      "I4081 46\n",
      "I4331 24\n",
      "I4332 18\n",
      "I4432 25\n",
      "I4434 21\n",
      "I4435 21\n",
      "I4436 13\n",
      "I4437 24\n",
      "I4438 14\n",
      "I4439 16\n",
      "I4440 26\n",
      "I4441 21\n",
      "I4550 23\n",
      "I4551 22\n",
      "I4552 18\n",
      "I4553 28\n",
      "I4554 20\n",
      "I4626 42\n",
      "I4627 20\n",
      "I4628 23\n",
      "I4629 49\n",
      "I4655 63\n",
      "I4657 62\n",
      "I4665 62\n",
      "I4666 60\n",
      "I0700 43\n",
      "I0704 53\n",
      "I0706 47\n",
      "I1108 47\n",
      "I1109 47\n",
      "I1113 47\n",
      "I1295 54\n",
      "I1296 56\n",
      "I1297 60\n",
      "I1733 46\n",
      "I1737 59\n",
      "I2175 38\n",
      "I2176 24\n",
      "I2181 55\n",
      "I2215 58\n",
      "I2216 60\n",
      "I2423 30\n",
      "I2424 26\n",
      "I2425 53\n",
      "I2426 61\n",
      "I2430 29\n",
      "I2526 39\n",
      "ILK001 14\n",
      "ILK002 12\n",
      "ILK003 11\n",
      "I1875 44\n",
      "I4595 19\n",
      "I4596 18\n",
      "I4630 19\n",
      "I4632 19\n",
      "I4871 39\n",
      "I4872 41\n",
      "I4873 16\n",
      "I4874 20\n",
      "I4875 18\n",
      "I4876 20\n",
      "I4877 17\n",
      "I4878 18\n",
      "I4880 16\n",
      "I4881 14\n",
      "I4882 18\n",
      "I4914 14\n",
      "I4915 17\n",
      "I4916 16\n",
      "I4917 19\n",
      "I4918 11\n",
      "I5068 47\n",
      "I5069 14\n",
      "I5070 16\n",
      "I5071 30\n",
      "I5072 27\n",
      "I5077 11\n",
      "I5078 14\n",
      "I5079 12\n",
      "I5232 18\n",
      "I5233 22\n",
      "I5234 25\n",
      "I5235 12\n",
      "I5236 13\n",
      "I5237 15\n",
      "I5238 14\n",
      "I5239 21\n",
      "I5240 18\n",
      "I5241 14\n",
      "I5242 19\n",
      "I5244 21\n",
      "I5204 16\n",
      "I5205 15\n",
      "I5206 17\n",
      "I5769 25\n",
      "I5771 54\n",
      "I5772 40\n",
      "I5773 62\n",
      "I5868 62\n",
      "I5869 55\n",
      "I5870 50\n",
      "I5872 59\n",
      "I5873 61\n",
      "I5875 35\n",
      "I5876 35\n",
      "I5878 58\n",
      "I5879 58\n",
      "I5881 58\n",
      "I5882 40\n",
      "I5883 54\n",
      "I5884 33\n",
      "I5885 48\n",
      "I5886 58\n",
      "I5888 54\n",
      "I5889 62\n",
      "I5890 45\n",
      "I5891 63\n",
      "I5892 58\n",
      "I5893 62\n",
      "I0679_d 45\n",
      "I2521 14\n",
      "I2529 26\n",
      "I5207 8\n",
      "I5208 10\n",
      "I5401 35\n",
      "I5402 16\n",
      "I5405 53\n",
      "I5407 15\n",
      "I5408 32\n",
      "I5409 31\n",
      "I5411 21\n",
      "I5427 20\n",
      "I5436 18\n",
      "I5957 55\n",
      "I6133 62\n",
      "I0698 17\n",
      "I1378 50\n",
      "I6561 25\n",
      "I2792 46\n",
      "I3708 18\n",
      "I3718 33\n",
      "I3709 13\n",
      "I4582 11\n",
      "I2318 33\n",
      "I4607 41\n",
      "I3920 8\n",
      "I4660 57\n",
      "I4870 28\n"
     ]
    }
   ],
   "source": [
    "#number of missing alleles\n",
    "count=0\n",
    "\n",
    "for j in range(len(commonsnps[0][9:])):\n",
    "    for i in range(len(commonsnps)): \n",
    "        if commonsnps[i][9 + j] == \"./.\":\n",
    "            count+=1\n",
    "    print(individuals[9 + j] + \" \" + str(count))\n",
    "    count=0\n",
    "    \n"
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.8.8"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 4
}
