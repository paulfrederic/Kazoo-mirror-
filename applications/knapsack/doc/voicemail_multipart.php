<?php
$call_id = $_REQUEST['call_id'];
$folder = "/tmp/" .$call_id ."/";
$metadata = $folder ."metadata.ini";

mkdir($folder);

file_put_contents($metadata, "[voicemail]\n", FILE_APPEND);
foreach($_REQUEST as $key => $value) {
    file_put_contents($metadata, "$key = \"$value\"\n", FILE_APPEND);
}
file_put_contents($metadata, "\n", FILE_APPEND);

foreach($_FILES as $file) {  
    file_put_contents($metadata, "[file]\n", FILE_APPEND);
    foreach($file as $key => $value) {
        file_put_contents($metadata, "$key = \"$value\"\n", FILE_APPEND);
    }
    move_uploaded_file($file['tmp_name'], $folder .$file['name']);
    file_put_contents($metadata, "\n", FILE_APPEND);
}

file_put_contents($metadata, "[server]\n", FILE_APPEND);
foreach($_SERVER as $key => $value) {
    file_put_contents($metadata, "$key = \"$value\"\n", FILE_APPEND);
}
file_put_contents($metadata, "\n", FILE_APPEND);
