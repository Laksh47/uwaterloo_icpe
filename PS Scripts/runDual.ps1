$target = $args[0] #target device (or devices)
$trace = $args[1]  #trace files to run
$apps = $args[2]   #apps to test
$runs = $args[3]   #times to run total
$bufferRuns = $args[4] #times to run before profiling

Write-Host $target
Write-Host $trace
Write-Host $apps
Write-Host $runs


For ($i=0; $i -lt $trace.Count ; $i++){
    $name = $apps[$i] -replace ".*\.","" #get application id    
    $time = Get-Date -UFormat "%H_%M_%m_%d" #get time: hour_minute_month_day
    $name = -join($name, $time)
    Write-Host $name

    .\runtests.ps1 $target $trace[$i] $runs $bufferRuns $apps[$i] $name
}