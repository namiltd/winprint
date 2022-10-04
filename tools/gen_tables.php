<?php
function toPAS($cp) {
    echo ' CP'.$cp."toUTF8: TUTF8Table=(\n   ";
    for ($i=0; $i<32; $i++) {
        for ($j=0; $j<4; $j++) {
            $c = 128+$i*4+$j;
            if ($cp==808) {
                $out=iconv('cp866','utf-8//IGNORE', chr($c));
            } elseif ($cp==858) {
                $out=iconv('cp850','utf-8//IGNORE', chr($c));
            } elseif ($cp==867) {
                $out=iconv('cp862','utf-8//IGNORE', chr($c));
            } elseif ($cp==872) {
                $out=iconv('cp855','utf-8//IGNORE', chr($c));
            } elseif ($cp==3021) {
                $out=iconv('mik','utf-8//IGNORE', chr($c));
            } elseif ($cp==20866) {
                $out=iconv('koi8-r','utf-8//IGNORE', chr($c));
            } elseif ($cp==21866) {
                $out=iconv('koi8-u','utf-8//IGNORE', chr($c));
            } elseif (($cp<28591)||($cp>28599)) {
                $out=iconv('cp'.$cp,'utf-8//IGNORE', chr($c));
            } else {
                $out=iconv('ISO-8859-'.($cp-28590),'utf-8//IGNORE', chr($c));
            }

            if ($out=='') {
                $out=chr(239).chr(191).chr(189);
            }

            if ((($cp==808)&&($c==253))
               ||(($cp==872)&&($c==207))
               ||(($cp==858)&&($c==213))) {
                $out=chr(226).chr(130).chr(172);
            } elseif($cp==867) {
                if ($c==159) $out=chr(226).chr(130).chr(170);
                elseif ($c==160) $out=chr(226).chr(128).chr(142);
                elseif ($c==161) $out=chr(226).chr(128).chr(143);
                elseif ($c==162) $out=chr(226).chr(128).chr(170);
                elseif ($c==163) $out=chr(226).chr(128).chr(171);
                elseif ($c==164) $out=chr(226).chr(128).chr(173);
                elseif ($c==165) $out=chr(226).chr(128).chr(174);
                elseif ($c==166) $out=chr(226).chr(128).chr(172);
                elseif ($c==173) $out=chr(226).chr(130).chr(172);
            }

            $chars=str_split($out);
            echo '(';
            $notfirst=false;
            foreach($chars as $char) {
                if ($notfirst) {
                    echo '+#$'.strtoupper(dechex(ord($char)));
                } else {
                    echo '#$'.strtoupper(dechex(ord($char)));
                    $notfirst=true;
                }
            }
            echo ')';
            if ($c!=255) echo ',';
        }
        if ($c!=255) echo "\n   ";
        else echo ");\n\n";
    }
}

toPAS(437);
//toPAS(620);
toPAS(737);
toPAS(775);
//toPAS(790);
toPAS(808);
toPAS(850);
toPAS(852);
toPAS(855);
toPAS(857);
toPAS(858);
toPAS(860);
toPAS(861);
toPAS(862);
toPAS(863);
toPAS(864);
toPAS(865);
toPAS(866);
toPAS(867);
//toPAS(868); different than ibm868
toPAS(869);
toPAS(872);
toPAS(874);
toPAS(1250);
toPAS(1251);
toPAS(1252);
toPAS(1253);
toPAS(1254);
toPAS(1255);
toPAS(1256);
toPAS(1257);
toPAS(1258);
toPAS(3021);
toPAS(20866);
toPAS(21866);
toPAS(28591);
toPAS(28592);
toPAS(28593);
toPAS(28594);
toPAS(28595);
toPAS(28596);
toPAS(28597);
toPAS(28598);
toPAS(28599);
