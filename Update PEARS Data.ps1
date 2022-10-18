$pearsdir = "S:\DataTeam\Pears\dataimport"
$boxdir = "C:\Users\user\path\to\communitynetworksdashboard\CommunityNetworksDashboard"

copy-item "$($pearsdir)\Coalition_Export.xlsx" "$boxdir\Coalition_Export.xlsx"
copy-item "$($pearsdir)\Indirect_Activity_Export.xlsx" "$boxdir\Indirect_Activity_Export.xlsx"
copy-item "$($pearsdir)\Partnership_Export.xlsx" "$boxdir\Partnership_Export.xlsx"
copy-item "$($pearsdir)\Program_Activities_Export.xlsx" "$boxdir\Program_Activities_Export.xlsx"
copy-item "$($pearsdir)\PSE_Site_Activity_Export.xlsx" "$boxdir\PSE_Site_Activity_Export.xlsx"
copy-item "$($pearsdir)\Site_Export.xlsx" "$boxdir\Site_Export.xlsx"