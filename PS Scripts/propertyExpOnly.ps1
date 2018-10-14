 
$target =  "ZY223MXRXQ"
$time = Get-Date -UFormat "%H_%M_%m_%d" #get time: hour_minute_month_day
 #currently just runs ionic

 $name = -join("IO_Prop_", $time)
.\propertyExp.ps1 `
    $target `
    "traces/scrolltest.trace" `
    15 5 `
    "com.IO_Prop" `
    $name
exit

$name = -join("AN_Prop_", $time)
Write-Host $name
.\propertyExp.ps1 `
    $target `
    "traces/scrolltest.trace" `
    15 5 `
    "com.AN_Prop" `
    $name

$name = -join("RN_Prop_", $time)
.\propertyExp.ps1 `
    $target `
    "traces/scrolltest.trace" `
    15 5 `
    "com.AN_Prop" `
    $name

