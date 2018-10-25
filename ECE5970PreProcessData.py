import os
from openpyxl import load_workbook
from openpyxl.styles import Font, Color

#Prior to running this code - Convert the .csv to .xlsx
#Change os.chdir('path') to where the 'TADPOLE_TargetData_train.xlsx' file is located

os.chdir('/Users/victoriatu/Desktop') #Set path to find/save file
workbook = load_workbook('TADPOLE_TargetData_validation.xlsx') #Get the .xlsx file
worksheet = workbook['TADPOLE_TargetData_validation'] #Pick the excel sheet to look at

for row in range(3,2507): #For each trial visit, or rows that have data
    
        #Characterize a clinical visit by the patient and diagnosis
        #Trial visit = [PTID_Key, CN_Diag, MCI_Diag, AD_Diag]
        before = [worksheet['B'+str(row-1)].value, worksheet['C'+str(row-1)].value, worksheet['D'+str(row-1)].value, worksheet['E'+str(row-1)].value]
        current = [worksheet['B'+str(row)].value, worksheet['C'+str(row)].value, worksheet['D'+str(row)].value, worksheet['E'+str(row)].value]
        
        #If the current row does not have a diagnosis, see if it is possible to infer diagnosis
        if current[1]=='NaN':
            index = row+1
            while (worksheet['C'+str(index)].value == 'NaN') and (current[0]==worksheet['B'+str(index)].value): #Find next row that has diagnosis
                 index = index + 1
            after = [worksheet['B'+str(index)].value, worksheet['C'+str(index)].value, worksheet['D'+str(index)].value, worksheet['E'+str(index)].value]
            if (current[0]==before[0]) and (current[0]==after[0]): #If the before and after rows are same person
                if (before[1]==after[1]) and (before[2]==after[2]) and (before[2]!='Nan'):#If the before and after row have same results
                    #Set the results to the trials in between
                    worksheet['C'+str(row)] = before[1]
                    worksheet['D'+str(row)] = before[2]
                    worksheet['E'+str(row)] = before[3]
#Save files
workbook.save('TADPOLE_TargetData_validation.xlsx')

