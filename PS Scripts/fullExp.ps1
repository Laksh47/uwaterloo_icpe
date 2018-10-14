$target = "ZY223MXRXQ"

#Maps
.\runDual.ps1 `
    $target `
    "traces/gmaps2.trace", "traces/gmaps2.trace", "traces/gmaps2.trace" `
    "com.AN_Maps", "com.RN_Maps", "com.IO_Maps" `
    2 1

#Matrix
.\runDual.ps1 `
    $target `
    "traces/matrix.trace", "traces/matrix.trace", "traces/matrix.trace" `
    "com.AN_Matr", "com.RN_Matr", "com.IO_Matr" `
    2 1


#Property

 $time = Get-Date -UFormat "%H_%M_%m_%d" #get time: hour_minute_month_day
 
$name = -join("AN_Prop", $time)
.\propertyExp.ps1 `
    $target `
    "traces/scrolltest.trace" `
    2 1 `
    "com.AN_Prop"
    $name

$name = -join("RN_Prop", $time)
.\propertyExp.ps1 `
    $target `
    "traces/scrolltest.trace" `
    2 1 `
    "com.AN_Prop" `
    $name

$name = -join("IO_Prop", $time)
.\propertyExp.ps1 `
    $target `
    "traces/scrolltest.trace" `
    2 1 `
    "com.IO_Prop" `
    $name
