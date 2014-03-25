<?php
$jobj = json_decode(file_get_contents('php://input'), true);

$call_id = $jobj['call_id'];
$folder = "/tmp/" .$call_id ."/";
$metadata = $folder ."metadata.ini";

mkdir($folder);

file_put_contents($metadata, "[voicemail]\n", FILE_APPEND);
foreach($jobj as $key => $value) {
    if ($key == 'content' || $key == 'name') {
        continue;
    }
    file_put_contents($metadata, "$key = \"$value\"\n", FILE_APPEND);
}
file_put_contents($metadata, "\n", FILE_APPEND);

file_put_contents($metadata, "[server]\n", FILE_APPEND);
foreach($_SERVER as $key => $value) {
    file_put_contents($metadata, "$key = \"$value\"\n", FILE_APPEND);
}
file_put_contents($metadata, "\n", FILE_APPEND);

file_put_contents($folder .$jobj['name'], base64_decode($jobj['content']));
