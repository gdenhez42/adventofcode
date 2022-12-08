<!DOCTYPE html>
<html>
<body>

<?php
class Folder {
    public $path;
    public $subfolders;
    public $filesizes;
    public function __construct($path) {
      $this->path = $path;
      $this->subfolders = [];
      $this->filesizes = 0;
    }
}

function calculateSize($folders, $folder) {
    $result = $folder->filesizes;
    foreach($folder->subfolders as $subfolder) {
        $path = "/".$subfolder;
        if ($folder->path !== "/") {
            $path = $folder->path . "/" . $subfolder;
        }
        $result += calculateSize($folders, $folders[$path]);
    }
    return $result;
}


$myfile = fopen("input.txt", "r") or die("Unable to open file!");
$input = fread($myfile,filesize("input.txt"));
fclose($myfile);

$lines = explode("\n", $input);
$current_path = [];
$current_folder = NULL;
$folders = [];
foreach($lines as $line) {
    $token = explode(" ", $line);
    if ($token[0] === "$") {
        if ($current_folder !== NULL) {
            $folders[$current_folder->path] = $current_folder;
            $current_folder = NULL;
        }

        if ($token[1] === "cd") {
            switch ($token[2]) {
                case "/":
                    $current_path = [];
                    break;
                case "..":
                    array_pop($current_path);
                    break;
                default:
                    array_push($current_path, $token[2]);
                    break;
            }
        }

        elseif ($token[1] == "ls") {
            $current_folder = new Folder("/" . implode("/",$current_path));
        }

    }

    elseif ($token[0] === "dir") {
        array_push($current_folder->subfolders, $token[1]);
    }

    else {
        $current_folder->filesizes += intval($token[0]);
    }
}
if ($current_folder !== NULL) {
    $folders[$current_folder->path] = $current_folder;
    $current_folder = NULL;
}

$part1 = 0;
foreach($folders as $folder) {
    $size = calculateSize($folders, $folder);
    if ($size < 100000) {
        $part1 += $size;
    }
}
echo "Part1: ".$part1 . "<br>";


$unusedSpace = 70000000 - calculateSize($folders, $folders["/"]);
$sizeNeeded = 30000000 - $unusedSpace;
$part2 = 70000000;
foreach($folders as $folder) {
    $size = calculateSize($folders, $folder);
    if ($size >= $sizeNeeded and $size < $part2) {
        $part2 = $size;
    }
}
echo "Part2: ".$part2 . "<br>";
?>

</body>
</html>

