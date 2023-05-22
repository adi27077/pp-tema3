{-
    PP Project 2022

    This file contains the dataset:
    - 4 tables, already formatted as [[String]]: emails, eight_hours, sleep_min, physical_activity.

    DO NOT MODIFY THIS FILE! If you suspect that anything is wrong, post on FORUM!!
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Dataset where

emails = [["Name","Email"],
    ["Olivia Noah","Olivia.Noah@stud.cs.pub.ro"],
    ["Riley Jackson","Riley.Jackson@stud.cs.pub.ro"],
    ["Emma Aiden","Emma.Aiden@stud.cs.pub.ro"],
    ["Eva Elijah","Ava.Elijah@stud.cs.pub.ro"],
    ["Isabela Grayson","Isabella.Grayson@stud.cs.pub.ro"],
    ["Aria Lucas","Aria.Lucas@stud.cs.pub.ro"],
    ["Aalyah Oliver","Aaliyah.Oliver@stud.cs.pub.ro"],
    ["Amelia Camdden","Amelia.Caden@stud.cs.pub.ro"],
    ["Mia Mateo","Mia.Mateo@stud.cs.pub.ro"],
    ["Layla Muhammad","Layla.Muhammad@stud.cs.pub.ro"],
    ["Zoe Mason","Zoe.Mason@stud.cs.pub.ro"],
    ["Camilla Carter","Camilla.Carter@stud.cs.pub.ro"],
    ["Charlote Jayden","Charlotte.Jayden@stud.cs.pub.ro"],
    ["Eliana Ethan","Eliana.Ethan@stud.cs.pub.ro"],
    ["Mila Sebastian","Mila.Sebastian@stud.cs.pub.ro"],
    ["Everrly James","Everly.James@stud.cs.pub.ro"],
    ["Luna Michael","Luna.Michael@stud.cs.pub.ro"],
    ["Avery Benjamin","Avery.Benjamin@stud.cs.pub.ro"],
    ["Evelyn Logan","Evelyn.Logan@stud.cs.pub.ro"],
    ["Harrper Leo","Harper.Leo@stud.cs.pub.ro"],
    ["Lilly Lucca","Lily.Luca@stud.cs.pub.ro"],
    ["Ella Alexander","Ella.Alexander@stud.cs.pub.ro"],
    ["Gianna Levi","Gianna.Levi@stud.cs.pub.ro"],
    ["Chloe Daniel","Chloe.Daniel@stud.cs.pub.ro"],
    ["Adalynne Josiah","Adalyn.Josiah@stud.cs.pub.ro"],
    ["Charlie Henry","Charlie.Henry@stud.cs.pub.ro"],
    ["Isla Jayce","Isla.Jayce@stud.cs.pub.ro"],
    ["Ellie Julian","Ellie.Julian@stud.cs.pub.ro"],
    ["Leah Jack","Leah.Jack@stud.cs.pub.ro"],
    ["Nora Ryan","Nora.Ryan@stud.cs.pub.ro"],
    ["Scarlet Jacob","Scarlett.Jacob@stud.cs.pub.ro"],
    ["Mayah Aser","Maya.Asher@stud.cs.pub.ro"],
    ["Abigail Wyatt","Abigail.Wyatt@stud.cs.pub.ro"],
    ["Madison Wiliam","Madison.William@stud.cs.pub.ro"],
    ["Aobrey Owen","Aubrey.Owen@stud.cs.pub.ro"],
    ["Emilly Gabrielle","Emily.Gabriel@stud.cs.pub.ro"],
    ["Kinsley Milos","Kinsley.Miles@stud.cs.pub.ro"],
    ["Elena Lincoln","Elena.Lincoln@stud.cs.pub.ro"],
    ["Paisley Ezrah","Paisley.Ezra@stud.cs.pub.ro"],
    ["Madelynne Isaiiah","Madelyn.Isaiah@stud.cs.pub.ro"],
    ["Aurora Luke","Aurora.Luke@stud.cs.pub.ro"],
    ["Peyton Cameron","Peyton.Cameron@stud.cs.pub.ro"],
    ["Novah Caleb","Nova.Caleb@stud.cs.pub.ro"],
    ["Emilia Isaac","Emilia.Isaac@stud.cs.pub.ro"],
    ["Hannah Carson","Hannah.Carson@stud.cs.pub.ro"],
    ["Sarah Samuel","Sarah.Samuel@stud.cs.pub.ro"],
    ["Ariana Colton","Ariana.Colton@stud.cs.pub.ro"],
    ["Penelope Maverick","Penelope.Maverick@stud.cs.pub.ro"],
    ["Lila Matthew","Lila.Matthew@stud.cs.pub.ro"],
    ["Brooklyn Ian","Brooklyn.Ian@stud.cs.pub.ro"],
    ["Emery David","Emery.David@stud.cs.pub.ro"],
    ["Callie Adam","Callie.Adam@stud.cs.pub.ro"],
    ["Hazel Nicholas","Hazel.Nicholas@stud.cs.pub.ro"],
    ["Hailey Elias","Hailey.Elias@stud.cs.pub.ro"],
    ["Eleanor Adrian","Eleanor.Adrian@stud.cs.pub.ro"],
    ["Violet Kai","Violet.Kai@stud.cs.pub.ro"],
    ["Elizabet Natan","Elizabeth.Nathan@stud.cs.pub.ro"],
    ["Grace Eli","Grace.Eli@stud.cs.pub.ro"],
    ["Anna Hudson","Anna.Hudson@stud.cs.pub.ro"],
    ["Mackenzie John","Mackenzie.John@stud.cs.pub.ro"],
    ["Kayle Zane","Kaylee.Zane@stud.cs.pub.ro"],
    ["Victoria Connor","Victoria.Connor@stud.cs.pub.ro"],
    ["Natalie Ezekiel","Natalie.Ezekiel@stud.cs.pub.ro"],
    ["Raelynn Anthony","Raelynn.Anthony@stud.cs.pub.ro"],
    ["Adison Josep","Addison.Joseph@stud.cs.pub.ro"],
    ["Skyler Landon","Skyler.Landon@stud.cs.pub.ro"],
    ["Melanie Jameson","Melanie.Jameson@stud.cs.pub.ro"],
    ["Stela Thomas","Stella.Thomas@stud.cs.pub.ro"],
    ["Naomi Aaron","Naomi.Aaron@stud.cs.pub.ro"],
    ["Lelani Christian","Lelani.Christian@stud.cs.pub.ro"],
    ["Isabele Havier","Isabelle.Xavier@stud.cs.pub.ro"],
    ["Bella Nolan","Bella.Nolan@stud.cs.pub.ro"],
    ["Liliana Easton","Liliana.Easton@stud.cs.pub.ro"],
    ["Kennedy Joshua","Kennedy.Joshua@stud.cs.pub.ro"],
    ["Gabriella Dominic","Gabriella.Dominic@stud.cs.pub.ro"],
    ["Willow Roman","Willow.Roman@stud.cs.pub.ro"],
    ["Lucie Dylan","Lucy.Dylan@stud.cs.pub.ro"],
    ["Savana Amir","Savannah.Amir@stud.cs.pub.ro"],
    ["Lillian Christopher","Lillian.Christopher@stud.cs.pub.ro"],
    ["Sophie Theodore","Sophie.Theodore@stud.cs.pub.ro"],
    ["Ivy Jeremiah","Ivy.Jeremiah@stud.cs.pub.ro"],
    ["Amaya Hunter","Amaya.Hunter@stud.cs.pub.ro"],
    ["Delilah Andrew","Delilah.Andrew@stud.cs.pub.ro"],
    ["Nevae Jordyn","Nevaeh.Jordyn@stud.cs.pub.ro"],
    ["London Damian","London.Damian@stud.cs.pub.ro"],
    ["Alaina Zion","Alaina.Zion@stud.cs.pub.ro"],
    ["Adeline Theo","Adeline.Theo@stud.cs.pub.ro"],
    ["Audrey Cooper","Audrey.Cooper@stud.cs.pub.ro"],
    ["Anaya Malachi","Anaya.Malachi@stud.cs.pub.ro"],
    ["Maria Axel","Maria.Axel@stud.cs.pub.ro"],
    ["Eva Santiago","Eva.Santiago@stud.cs.pub.ro"],
    ["Jasmine Elliot","Jasmine.Elliot@stud.cs.pub.ro"],
    ["Nyla Brison","Nyla.Bryson@stud.cs.pub.ro"],
    ["Cora Brayden","Cora.Brayden@stud.cs.pub.ro"],
    ["Reagan Roawan","Reagan.Rowan@stud.cs.pub.ro"],
    ["Makaila Jonathan","Makayla.Jonathan@stud.cs.pub.ro"],
    ["Claire Evan","Claire.Evan@stud.cs.pub.ro"],
    ["Allison Max","Allison.Max@stud.cs.pub.ro"],
    ["Emerson Beau","Emerson.Beau@stud.cs.pub.ro"],
    ["Liam Sophia","Liam.Sophia@stud.cs.pub.ro"],
    ["Noah Olivia","Noah.Olivia@stud.cs.pub.ro"],
    ["Jacson Riley","Jackson.Riley@stud.cs.pub.ro"],
    ["Aiden Emma","Aiden.Emma@stud.cs.pub.ro"],
    ["Elijah Eva","Elijah.Ava@stud.cs.pub.ro"],
    ["Grayson Isabella","Grayson.Isabella@stud.cs.pub.ro"],
    ["Lucas Aria","Lucas.Aria@stud.cs.pub.ro"],
    ["Oliver Aaliyah","Oliver.Aaliyah@stud.cs.pub.ro"],
    ["Caden Amelia","Caden.Amelia@stud.cs.pub.ro"],
    ["Mateo Mia","Mateo.Mia@stud.cs.pub.ro"],
    ["Muhammad Layla","Muhammad.Layla@stud.cs.pub.ro"],
    ["Mason Zoe","Mason.Zoe@stud.cs.pub.ro"],
    ["Carter Camila","Carter.Camilla@stud.cs.pub.ro"],
    ["Jaden Charlote","Jayden.Charlotte@stud.cs.pub.ro"],
    ["Ethan Eliana","Ethan.Eliana@stud.cs.pub.ro"],
    ["Sebastian Mila","Sebastian.Mila@stud.cs.pub.ro"],
    ["James Everly","James.Everly@stud.cs.pub.ro"],
    ["Michael Luna","Michael.Luna@stud.cs.pub.ro"],
    ["Beniamin Avery","Benjamin.Avery@stud.cs.pub.ro"],
    ["Logan Evelin","Logan.Evelyn@stud.cs.pub.ro"],
    ["Leo Harper","Leo.Harper@stud.cs.pub.ro"],
    ["Luca Lily","Luca.Lily@stud.cs.pub.ro"],
    ["Alexander Ela","Alexander.Ella@stud.cs.pub.ro"],
    ["Levi Gianna","Levi.Gianna@stud.cs.pub.ro"],
    ["Daniel Chloe","Daniel.Chloe@stud.cs.pub.ro"],
    ["Josiah Adalinne","Josiah.Adalyn@stud.cs.pub.ro"],
    ["Henry Charlie","Henry.Charlie@stud.cs.pub.ro"],
    ["Jayce Isla","Jayce.Isla@stud.cs.pub.ro"],
    ["Julian Ellie","Julian.Ellie@stud.cs.pub.ro"],
    ["Jack Leah","Jack.Leah@stud.cs.pub.ro"],
    ["Ryan Nora","Ryan.Nora@stud.cs.pub.ro"],
    ["Jacob Scarlett","Jacob.Scarlett@stud.cs.pub.ro"],
    ["Asher Maya","Asher.Maya@stud.cs.pub.ro"],
    ["Wyatt Abigail","Wyatt.Abigail@stud.cs.pub.ro"],
    ["William Madison","William.Madison@stud.cs.pub.ro"],
    ["Owen Aubray","Owen.Aubrey@stud.cs.pub.ro"],
    ["Gabriel Emily","Gabriel.Emily@stud.cs.pub.ro"],
    ["Miles Kinsley","Miles.Kinsley@stud.cs.pub.ro"],
    ["Lincoln Elena","Lincoln.Elena@stud.cs.pub.ro"],
    ["Ezra Paisley","Ezra.Paisley@stud.cs.pub.ro"],
    ["Isaiah Madelyn","Isaiah.Madelyn@stud.cs.pub.ro"],
    ["Luke Aurora","Luke.Aurora@stud.cs.pub.ro"],
    ["Cameron Peyton","Cameron.Peyton@stud.cs.pub.ro"],
    ["Caleb Nova","Caleb.Nova@stud.cs.pub.ro"],
    ["Isaac Emilia","Isaac.Emilia@stud.cs.pub.ro"],
    ["Carson Hannah","Carson.Hannah@stud.cs.pub.ro"],
    ["Samuel Sarah","Samuel.Sarah@stud.cs.pub.ro"],
    ["Colton Ariana","Colton.Ariana@stud.cs.pub.ro"],
    ["Maverick Penelopa","Maverick.Penelope@stud.cs.pub.ro"],
    ["Matthew Lila","Matthew.Lila@stud.cs.pub.ro"],
    ["Ian Brooklyn","Ian.Brooklyn@stud.cs.pub.ro"],
    ["David Emery","David.Emery@stud.cs.pub.ro"],
    ["Adam Callie","Adam.Callie@stud.cs.pub.ro"],
    ["Nicholas Hazel","Nicholas.Hazel@stud.cs.pub.ro"],
    ["Elias Hailey","Elias.Hailey@stud.cs.pub.ro"],
    ["Adrian Eleanor","Adrian.Eleanor@stud.cs.pub.ro"],
    ["Kai Violet","Kai.Violet@stud.cs.pub.ro"],
    ["Nathan Elizabeth","Nathan.Elizabeth@stud.cs.pub.ro"],
    ["Eli Grace","Eli.Grace@stud.cs.pub.ro"],
    ["Hudson Anna","Hudson.Anna@stud.cs.pub.ro"],
    ["John Mackenzie","John.Mackenzie@stud.cs.pub.ro"],
    ["Zan Kayle","Zane.Kaylee@stud.cs.pub.ro"],
    ["Connor Victoria","Connor.Victoria@stud.cs.pub.ro"],
    ["Ezekielle Natalie","Ezekiel.Natalie@stud.cs.pub.ro"],
    ["Anthony Raelynn","Anthony.Raelynn@stud.cs.pub.ro"],
    ["Joseph Addison","Joseph.Addison@stud.cs.pub.ro"],
    ["Landon Skyler","Landon.Skyler@stud.cs.pub.ro"],
    ["Jameson Melanie","Jameson.Melanie@stud.cs.pub.ro"],
    ["Thomas Stella","Thomas.Stella@stud.cs.pub.ro"],
    ["Aaron Naomi","Aaron.Naomi@stud.cs.pub.ro"],
    ["Christian Lelani","Christian.Lelani@stud.cs.pub.ro"],
    ["Xavier Isabelle","Xavier.Isabelle@stud.cs.pub.ro"],
    ["Nolan Bella","Nolan.Bella@stud.cs.pub.ro"],
    ["Easton Liliana","Easton.Liliana@stud.cs.pub.ro"],
    ["Joshua Kennedy","Joshua.Kennedy@stud.cs.pub.ro"]]

eight_hours = [["Name","10","11","12","13","14","15","16","17"],
    ["Olivia Noah","373","160","151","0","0","0","0","0"],
    ["Riley Jackson","31","0","0","7","0","0","0","0"],
    ["Emma Aiden","45","8","0","0","0","0","0","0"],
    ["Aria Lucas","0","0","0","0","0","0","0","0"],
    ["Aaliyah Oliver","0","0","0","0","4","0","20","0"],
    ["Amelia Caden","0","0","0","0","0","0","0","847"],
    ["Layla Muhammad","29","0","0","0","0","0","319","225"],
    ["Zoe Mason","0","0","0","0","0","0","0","497"],
    ["Camilla Carter","0","0","0","0","0","0","0","397"],
    ["Charlotte Jayden","0","0","0","0","0","306","17","1373"],
    ["Mila Sebastian","0","0","0","0","0","0","95","46"],
    ["Luna Michael","753","0","0","16","0","0","0","0"],
    ["Avery Benjamin","0","0","0","0","8","0","52","190"],
    ["Evelyn Logan","7","0","0","156","0","0","24","432"],
    ["Lily Luca","0","0","0","0","0","0","0","0"],
    ["Ella Alexander","0","0","0","0","0","0","0","0"],
    ["Gianna Levi","0","88","0","42","15","0","0","73"],
    ["Adalyn Josiah","0","0","0","0","0","0","151","208"],
    ["Charlie Henry","0","0","0","4","0","0","558","177"],
    ["Ellie Julian","2","0","0","0","0","0","248","2030"],
    ["Scarlett Jacob","0","0","0","0","0","31","2073","112"],
    ["Maya Asher","0","0","0","0","0","0","0","0"],
    ["Abigail Wyatt","0","0","0","0","0","0","54","517"],
    ["Madison William","0","0","0","0","0","0","0","0"],
    ["Aubrey Owen","32","0","0","0","0","0","21","412"],
    ["Emily Gabriel","0","0","0","0","0","0","881","3855"],
    ["Kinsley Miles","0","0","0","0","0","0","0","422"],
    ["Elena Lincoln","0","7","283","0","0","0","0","0"],
    ["Paisley Ezra","0","0","0","0","0","0","0","0"],
    ["Madelyn Isaiah","0","51","0","0","289","0","322","735"],
    ["Aurora Luke","0","0","0","0","0","0","0","0"],
    ["Peyton Cameron","0","0","7","0","0","0","0","0"],
    ["Emilia Isaac","0","0","0","0","0","0","209","964"],
    ["Hannah Carson","2367","202","833","557","0","0","0","7"],
    ["Penelope Maverick","0","0","0","480","33","0","0","428"],
    ["Lila Matthew","2168","259","0","0","0","0","263","72"],
    ["Emery David","0","324","0","0","0","0","1138","718"],
    ["Callie Adam","0","430","272","1832","53","7","523","292"],
    ["Hazel Nicholas","282","0","548","0","0","306","270","0"],
    ["Hailey Elias","180","31","564","368","0","45","0","0"],
    ["Violet Kai","876","258","125","227","6","7","67","0"],
    ["Elizabeth Nathan","0","805","826","394","476","12","433","146"],
    ["Grace Eli","556","1417","0","0","19","1131","99","0"],
    ["Anna Hudson","0","688","7","386","0","185","777","577"],
    ["Mackenzie John","643","179","440","601","1179","4688","0","2670"],
    ["Kaylee Zane","53","431","0","0","2284","361","587","25"],
    ["Victoria Connor","30","0","88","0","1470","194","0","67"],
    ["Natalie Ezekiel","0","209","326","0","1720","500","0","0"],
    ["Raelynn Anthony","494","10","1332","383","0","0","0","0"],
    ["Addison Joseph","0","238","564","0","0","8","5786","253"],
    ["Stella Thomas","2273","0","0","59","472","0","636","71"],
    ["Naomi Aaron","0","256","384","0","0","130","0","459"],
    ["Lelani Christian","619","224","0","0","159","637","411","415"],
    ["Isabelle Xavier","173","271","0","49","1302","812","550","864"],
    ["Bella Nolan","419","12","1719","0","839","342","357","0"],
    ["Liliana Easton","564","4","855","0","0","280","0","226"],
    ["Kennedy Joshua","204","0","460","310","46","150","0","3181"],
    ["Gabriella Dominic","2175","0","0","0","1404","0","693","195"],
    ["Willow Roman","2023","0","0","0","0","584","142","1615"],
    ["Lucy Dylan","252","62","723","962","9062","0","1259","90"],
    ["Savannah Amir","1620","6","3955","243","682","462","17","0"],
    ["Lillian Christopher","116","0","86","13","39","0","0","860"],
    ["Sophie Theodore","488","0","77","0","0","0","0","0"],
    ["Delilah Andrew","144","709","247","30","0","0","546","0"],
    ["London Damian","10","0","729","477","52","0","37","13"],
    ["Alaina Zion","81","0","0","1135","0","3525","0","0"],
    ["Adeline Theo","871","313","15","854","0","0","19","1474"],
    ["Audrey Cooper","111","48","0","2344","9","0","72","0"],
    ["Anaya Malachi","629","1112","51","0","0","654","1333","1632"],
    ["Maria Axel","0","0","425","0","319","0","0","615"],
    ["Eva Santiago","326","304","0","0","0","227","0","647"],
    ["Jasmine Elliot","4","1460","0","338","0","0","0","337"],
    ["Cora Brayden","100","0","90","92","1552","0","844","323"],
    ["Claire Evan","0","578","0","3389","0","0","0","0"],
    ["Allison Max","5","2925","0","0","360","635","0","0"],
    ["Liam Sophia","0","159","67","459","1460","1071","35","824"],
    ["Noah Olivia","1383","0","0","0","933","0","255","0"],
    ["Jackson Riley","1303","445","0","643","56","3764","13","251"],
    ["Aiden Emma","34","506","904","0","0","403","0","0"],
    ["Grayson Isabella","0","0","462","491","234","11","0","0"],
    ["Oliver Aaliyah","218","119","166","1911","81","304","0","1048"],
    ["Caden Amelia","637","1140","0","0","0","50","0","861"],
    ["Mateo Mia","45","626","0","0","0","652","1170","0"],
    ["Muhammad Layla","0","0","78","16","0","0","1093","762"],
    ["Mason Zoe","0","0","144","604","4","0","0","0"],
    ["Ethan Eliana","0","275","0","0","549","6","140","292"],
    ["Sebastian Mila","0","0","0","2669","0","0","16","0"],
    ["James Everly","94","0","0","9","0","0","0","1061"],
    ["Michael Luna","0","50","1717","765","0","22","0","1657"],
    ["Benjamin Avery","0","0","3771","0","707","147","496","0"],
    ["Logan Evelyn","0","839","0","140","402","669","269","110"],
    ["Luca Lily","293","0","54","225","155","0","328","0"],
    ["Alexander Ella","0","0","4","0","0","133","0","0"],
    ["Levi Gianna","35","96","127","0","110","283","178","399"],
    ["Daniel Chloe","743","0","0","187","0","616","267","1404"],
    ["Julian Ellie","580","406","170","169","0","37","634","82"],
    ["Jack Leah","438","0","32","5","0","58","0","1054"],
    ["Ryan Nora","0","234","299","488","185","907","395","796"],
    ["Jacob Scarlett","0","441","29","1058","838","707","9","684"],
    ["Asher Maya","16","557","458","0","498","695","136","162"],
    ["William Madison","0","815","451","1253","0","0","94","198"],
    ["Owen Aubrey","286","0","642","456","25","42","0","346"],
    ["Gabriel Emily","1120","103","0","52","54","812","399","9"],
    ["Ezra Paisley","283","1093","0","0","646","271","367","194"],
    ["Isaiah Madelyn","615","0","271","271","0","0","0","226"],
    ["Luke Aurora","0","14","480","167","53","0","344","223"],
    ["Cameron Peyton","0","85","109","0","289","283","0","0"],
    ["Caleb Nova","652","160","1663","0","108","48","10","2438"],
    ["Isaac Emilia","0","153","214","105","1185","419","1112","0"],
    ["Samuel Sarah","0","53","0","88","1564","12","72","0"],
    ["Colton Ariana","34","0","0","0","0","0","58","0"],
    ["Matthew Lila","821","0","38","355","326","374","12","0"],
    ["Ian Brooklyn","0","4","72","639","0","0","788","0"],
    ["David Emery","159","2128","12","1211","570","0","247","214"],
    ["Adam Callie","372","128","0","5","209","0","502","22"],
    ["Nicholas Hazel","0","0","0","4","0","0","1554","50"],
    ["Elias Hailey","166","0","9","1795","244","353","0","0"],
    ["Adrian Eleanor","0","0","881","645","147","161","0","44"],
    ["Kai Violet","0","62","182","1500","0","0","0","0"],
    ["Eli Grace","0","0","506","121","0","179","901","79"],
    ["Hudson Anna","45","132","0","0","0","171","0","178"],
    ["Zane Kaylee","0","0","299","0","1227","0","21","3807"],
    ["Connor Victoria","341","3251","35","569","137","78","0","0"],
    ["Anthony Raelynn","92","121","595","0","655","625","565","820"],
    ["Joseph Addison","342","0","0","315","118","133","4060","789"],
    ["Landon Skyler","0","174","0","1641","152","0","0","286"],
    ["Thomas Stella","274","353","271","0","0","783","0","1577"],
    ["Aaron Naomi","0","0","0","0","0","0","407","0"],
    ["Christian Lelani","359","322","101","360","0","0","1139","1004"],
    ["Xavier Isabelle","464","0","1468","2767","45","1454","0","941"],
    ["Easton Liliana","0","316","169","405","440","137","265","0"],
    ["Joshua Kennedy","183","75","419","0","1112","0","0","0"]]

physical_activity = [["Name","TotalSteps","TotalDistance","VeryActiveMinutes","FairlyActiveMinutes","LightlyActiveMinutes"],
    ["Olivia Noah","13162","8.50","25","13","328"],
    ["Riley Jackson","10735","6.97","21","19","217"],
    ["Emma Aiden","10460","6.74","30","11","181"],
    ["Ava Elijah","9762","6.28","29","34","209"],
    ["Isabella Grayson","12669","8.16","36","10","221"],
    ["Aria Lucas","9705","6.48","38","20","164"],
    ["Aaliyah Oliver","13019","8.59","42","16","233"],
    ["Amelia Caden","15506","9.88","50","31","264"],
    ["Mia Mateo","10544","6.68","28","12","205"],
    ["Layla Muhammad","9819","6.34","19","8","211"],
    ["Zoe Mason","12764","8.13","66","27","130"],
    ["Camilla Carter","14371","9.04","41","21","262"],
    ["Charlotte Jayden","10039","6.41","39","5","238"],
    ["Eliana Ethan","15355","9.80","73","14","216"],
    ["Mila Sebastian","13755","8.79","31","23","279"],
    ["Everly James","18134","12.21","78","11","243"],
    ["Luna Michael","13154","8.53","48","28","189"],
    ["Avery Benjamin","11181","7.15","16","12","243"],
    ["Evelyn Logan","14673","9.25","52","34","217"],
    ["Harper Leo","10602","6.81","33","35","246"],
    ["Lily Luca","14727","9.71","41","15","277"],
    ["Ella Alexander","15103","9.66","50","24","254"],
    ["Gianna Levi","11100","7.15","36","22","203"],
    ["Chloe Daniel","14070","8.90","45","24","250"],
    ["Adalyn Josiah","12159","8.03","24","6","289"],
    ["Charlie Henry","11992","7.71","37","46","175"],
    ["Isla Jayce","10060","6.58","44","8","203"],
    ["Ellie Julian","12022","7.72","46","11","206"],
    ["Leah Jack","12207","7.77","46","31","214"],
    ["Nora Ryan","12770","8.13","36","23","251"],
    ["Scarlett Jacob","0","0.00","0","0","0"],
    ["Maya Asher","8163","5.31","0","0","146"],
    ["Abigail Wyatt","7007","4.55","0","0","148"],
    ["Madison William","9107","5.92","0","0","236"],
    ["Aubrey Owen","1510","0.98","0","0","96"],
    ["Emily Gabriel","5370","3.49","0","0","176"],
    ["Kinsley Miles","6175","4.06","15","22","127"],
    ["Elena Lincoln","10536","7.41","17","7","202"],
    ["Paisley Ezra","2916","1.90","0","0","141"],
    ["Madelyn Isaiah","4974","3.23","0","0","151"],
    ["Aurora Luke","6349","4.13","0","0","186"],
    ["Peyton Cameron","4026","2.62","0","0","199"],
    ["Nova Caleb","8538","5.55","0","0","227"],
    ["Emilia Isaac","6076","3.95","16","18","185"],
    ["Hannah Carson","6497","4.22","0","0","202"],
    ["Sarah Samuel","2826","1.84","0","0","140"],
    ["Ariana Colton","8367","5.44","17","36","154"],
    ["Penelope Maverick","2759","1.79","0","5","115"],
    ["Lila Matthew","2390","1.55","0","0","150"],
    ["Brooklyn Ian","6474","4.30","11","23","224"],
    ["Emery David","36019","28.03","186","63","171"],
    ["Callie Adam","7155","4.93","7","6","166"],
    ["Hazel Nicholas","2100","1.37","0","0","96"],
    ["Hailey Elias","2193","1.43","0","0","118"],
    ["Eleanor Adrian","2470","1.61","0","0","117"],
    ["Violet Kai","1727","1.12","0","0","102"],
    ["Elizabeth Nathan","2104","1.37","0","0","182"],
    ["Grace Eli","3427","2.23","0","0","152"],
    ["Anna Hudson","1732","1.13","0","0","91"],
    ["Mackenzie John","2969","1.93","0","0","139"],
    ["Kaylee Zane","3134","2.04","0","0","112"],
    ["Victoria Connor","2971","1.93","0","0","107"],
    ["Natalie Ezekiel","10694","7.77","2","51","256"],
    ["Raelynn Anthony","8001","5.82","30","16","135"],
    ["Addison Joseph","11037","8.02","5","58","252"],
    ["Skyler Landon","5263","3.83","3","4","170"],
    ["Melanie Jameson","15300","11.12","51","42","212"],
    ["Stella Thomas","8757","6.37","29","13","186"],
    ["Naomi Aaron","7132","5.19","15","33","121"],
    ["Lelani Christian","11256","8.18","5","58","278"],
    ["Isabelle Xavier","2436","1.77","0","0","125"],
    ["Bella Nolan","1223","0.89","0","0","38"],
    ["Liliana Easton","3673","2.67","0","0","86"],
    ["Kennedy Joshua","6637","4.83","0","15","160"],
    ["Gabriella Dominic","3321","2.41","0","0","89"],
    ["Willow Roman","3580","2.60","8","1","94"],
    ["Lucy Dylan","9919","7.21","11","41","223"],
    ["Savannah Amir","3032","2.20","0","0","118"],
    ["Lillian Christopher","9405","6.84","3","53","227"],
    ["Sophie Theodore","3176","2.31","0","0","120"],
    ["Ivy Jeremiah","18213","13.24","9","71","402"],
    ["Amaya Hunter","6132","4.46","3","24","146"],
    ["Delilah Andrew","3758","2.73","1","7","148"],
    ["Nevaeh Jordyn","12850","9.34","10","94","221"],
    ["London Damian","2309","1.68","0","0","52"],
    ["Alaina Zion","4363","3.19","6","12","81"],
    ["Adeline Theo","9787","7.12","11","6","369"],
    ["Audrey Cooper","13372","9.72","41","17","243"],
    ["Anaya Malachi","6724","4.89","0","0","295"],
    ["Maria Axel","6643","4.83","32","6","303"],
    ["Eva Santiago","9167","6.66","12","19","155"],
    ["Jasmine Elliot","1329","0.97","0","0","49"],
    ["Nyla Bryson","6697","4.43","0","0","339"],
    ["Cora Brayden","4929","3.26","0","0","248"],
    ["Reagan Rowan","7937","5.25","0","0","373"],
    ["Makayla Jonathan","3844","2.54","0","0","176"],
    ["Claire Evan","3414","2.26","0","0","147"],
    ["Allison Max","4525","2.99","2","8","199"],
    ["Emerson Beau","4597","3.04","0","12","217"],
    ["Liam Sophia","197","0.13","0","0","10"],
    ["Noah Olivia","8","0.01","0","0","1"],
    ["Jackson Riley","8054","5.32","2","13","308"],
    ["Aiden Emma","5372","3.55","0","0","220"],
    ["Elijah Ava","3570","2.36","0","0","139"],
    ["Grayson Isabella","0","0.00","0","0","0"],
    ["Lucas Aria","0","0.00","0","0","0"],
    ["Oliver Aaliyah","0","0.00","0","0","0"],
    ["Caden Amelia","4","0.00","0","0","1"],
    ["Mateo Mia","6907","4.57","0","0","302"],
    ["Muhammad Layla","4920","3.25","0","0","247"],
    ["Mason Zoe","4014","2.67","0","0","184"],
    ["Carter Camilla","2573","1.70","0","7","75"],
    ["Jayden Charlotte","0","0.00","0","0","0"],
    ["Ethan Eliana","4059","2.68","0","0","184"],
    ["Sebastian Mila","2080","1.37","0","0","87"],
    ["James Everly","2237","1.48","0","0","120"],
    ["Michael Luna","44","0.03","0","0","2"],
    ["Benjamin Avery","0","0.00","0","0","0"],
    ["Logan Evelyn","0","0.00","0","0","0"],
    ["Leo Harper","0","0.00","0","0","0"],
    ["Luca Lily","0","0.00","0","0","0"],
    ["Alexander Ella","0","0.00","0","0","0"],
    ["Levi Gianna","0","0.00","0","0","0"],
    ["Daniel Chloe","678","0.47","0","0","55"],
    ["Josiah Adalyn","356","0.25","0","0","32"],
    ["Henry Charlie","2163","1.50","0","9","88"],
    ["Jayce Isla","980","0.68","0","0","51"],
    ["Julian Ellie","0","0.00","0","0","0"],
    ["Jack Leah","0","0.00","0","0","0"],
    ["Ryan Nora","244","0.17","0","0","17"],
    ["Jacob Scarlett","0","0.00","0","0","0"],
    ["Asher Maya","0","0.00","0","0","0"],
    ["Wyatt Abigail","0","0.00","0","0","0"],
    ["William Madison","149","0.10","0","0","10"],
    ["Owen Aubrey","2945","2.04","0","0","145"],
    ["Gabriel Emily","2090","1.45","1","6","75"],
    ["Miles Kinsley","152","0.11","0","0","12"],
    ["Lincoln Elena","3761","2.60","0","0","192"],
    ["Ezra Paisley","0","0.00","0","0","0"],
    ["Isaiah Madelyn","1675","1.16","0","0","95"],
    ["Luke Aurora","0","0.00","0","0","0"],
    ["Cameron Peyton","0","0.00","0","0","0"],
    ["Caleb Nova","2704","1.87","14","1","70"],
    ["Isaac Emilia","3790","2.62","16","8","94"],
    ["Carson Hannah","1326","0.92","10","0","17"],
    ["Samuel Sarah","1786","1.24","0","0","87"],
    ["Colton Ariana","0","0.00","0","0","0"],
    ["Maverick Penelope","2091","1.45","0","0","108"],
    ["Matthew Lila","1510","1.04","0","0","48"],
    ["Ian Brooklyn","0","0.00","0","0","0"],
    ["David Emery","0","0.00","0","0","0"],
    ["Adam Callie","0","0.00","0","0","0"],
    ["Nicholas Hazel","0","0.00","0","0","0"],
    ["Elias Hailey","0","0.00","0","0","0"],
    ["Adrian Eleanor","11875","8.34","42","14","227"],
    ["Kai Violet","12024","8.50","43","5","292"],
    ["Nathan Elizabeth","10690","7.50","32","3","257"],
    ["Eli Grace","11034","8.03","27","9","282"],
    ["Hudson Anna","10100","7.09","41","11","151"],
    ["John Mackenzie","15112","11.40","28","29","331"],
    ["Zane Kaylee","14131","10.07","48","3","311"],
    ["Connor Victoria","11548","8.53","31","7","250"],
    ["Ezekiel Natalie","15112","10.67","48","63","276"],
    ["Anthony Raelynn","12453","8.74","104","53","255"],
    ["Joseph Addison","12954","9.33","52","10","273"],
    ["Landon Skyler","6001","4.21","0","0","249"],
    ["Jameson Melanie","13481","10.28","37","26","216"],
    ["Thomas Stella","11369","8.01","44","8","217"],
    ["Aaron Naomi","10119","7.19","55","24","275"],
    ["Christian Lelani","10159","7.13","19","20","282"],
    ["Xavier Isabelle","10140","7.12","6","20","291"],
    ["Nolan Bella","10245","7.19","21","40","281"],
    ["Easton Liliana","18387","12.91","13","23","361"],
    ["Joshua Kennedy","10538","7.40","25","28","245"]]

sleep_min = [["Email","TotalMinutesAsleep1","TotalMinutesAsleep2","TotalMinutesAsleep3","TotalMinutesAsleep4","TotalMinutesAsleep5","TotalMinutesAsleep6","TotalMinutesAsleep7"],
    ["Mason.Zoe@stud.cs.pub.ro","327","384","412","340","700","304","360"],
    ["Ian.Brooklyn@stud.cs.pub.ro","119","124","796","137","0","0","0"],
    ["Lucy.Dylan@stud.cs.pub.ro","644","722","590","0","0","0","0"],
    ["Lucas.Aria@stud.cs.pub.ro","750","398","475","296","166","0","0"],
    ["Joshua.Kennedy@stud.cs.pub.ro","503","531","545","523","524","437","498"],
    ["Alexander.Ella@stud.cs.pub.ro","61","0","0","0","0","0","0"],
    ["Kennedy.Joshua@stud.cs.pub.ro","467","445","452","556","500","465","460"],
    ["Sophie.Theodore@stud.cs.pub.ro","274","295","291","424","283","381","412"],
    ["Layla.Muhammad@stud.cs.pub.ro","501","77","322","478","226","385","364"],
    ["Zane.Kaylee@stud.cs.pub.ro","535","465","506","515","461","523","59"],
    ["Olivia.Noah@stud.cs.pub.ro","499","426","619","99","329","421","442"],
    ["Emily.Gabriel@stud.cs.pub.ro","429","370","441","337","462","98","388"],
    ["Madison.William@stud.cs.pub.ro","126","103","171","115","123","0","0"],
    ["Scarlett.Jacob@stud.cs.pub.ro","425","400","384","253","382","591","293"],
    ["Caleb.Nova@stud.cs.pub.ro","441","455","357","377","651","350","520"],
    ["Isaac.Emilia@stud.cs.pub.ro","419","432","477","392","406","549","527"],
    ["Addison.Joseph@stud.cs.pub.ro","380","336","493","465","474","508","480"],
    ["Hazel.Nicholas@stud.cs.pub.ro","235","423","391","0","0","0","0"],
    ["Abigail.Wyatt@stud.cs.pub.ro","366","630","508","370","357","427","442"],
    ["Violet.Kai@stud.cs.pub.ro","79","58","0","0","0","0","0"],
    ["Eli.Grace@stud.cs.pub.ro","514","451","472","377","472","492","390"],
    ["Logan.Evelyn@stud.cs.pub.ro","486","331","74","0","0","0","0"],
    ["Ezra.Paisley@stud.cs.pub.ro","338","447","424","513","611","525","398"],
    ["Ella.Alexander@stud.cs.pub.ro","458","531","486","363","528","391","339"],
    ["Luca.Lily@stud.cs.pub.ro","722","486","475","700","332","468","488"],
    ["","420","291","447","428","441","486","439"],
    ["Audrey.Cooper@stud.cs.pub.ro","369","379","556","425","503","472","474"],
    ["Thomas.Stella@stud.cs.pub.ro","465","531","370","488","523","354","506"],
    ["Aaron.Naomi@stud.cs.pub.ro","360","405","433","411","377","437","467"],
    ["Grace.Eli@stud.cs.pub.ro","322","219","355","533","573","398","484"],
    ["Elena.Lincoln@stud.cs.pub.ro","631","503","347","354","360","343","399"],
    ["Josiah.Adalyn@stud.cs.pub.ro","412","137","472","427","430","602","338"],
    ["Hailey.Elias@stud.cs.pub.ro","327","527","400","472","340","374","507"],
    ["","442","106","250","293","398","404","471"],
    ["Eva.Santiago@stud.cs.pub.ro","355","471","485","398","508","474","417"],
    ["Luna.Michael@stud.cs.pub.ro","363","421","171","354","364","355","126"],
    ["Asher.Maya@stud.cs.pub.ro","285","421","296","442","492","412","394"],
    ["Mila.Sebastian@stud.cs.pub.ro","452","363","545","336","412","478","514"],
    ["Mateo.Mia@stud.cs.pub.ro","77","465","387","523","82","459","481"],
    ["Emma.Aiden@stud.cs.pub.ro","441","520","555","498","98","478","515"],
    ["Joseph.Addison@stud.cs.pub.ro","750","74","439","651","498","370","542"],
    ["Muhammad.Layla@stud.cs.pub.ro","409","611","555","377","250","409","425"],
    ["Elias.Hailey@stud.cs.pub.ro","439","277","547","388","511","426","364"],
    ["Owen.Aubrey@stud.cs.pub.ro","450","602","445","439","631","467","543"],
    ["Victoria.Connor@stud.cs.pub.ro","409","273","261","441","259","405","237"],
    ["Stella.Thomas@stud.cs.pub.ro","441","409","126","429","436","439","274"],
    ["Ryan.Nora@stud.cs.pub.ro","465","245","483","451","414","404","524"],
    ["Melanie.Jameson@stud.cs.pub.ro","478","443","523","423","400","543","366"],
    ["Xavier.Isabelle@stud.cs.pub.ro","412","123","436","457","405","338","337"],
    ["William.Madison@stud.cs.pub.ro","354","465","414","405","441","492","449"],
    ["","411","543","421","277","384","74","342"],
    ["Noah.Olivia@stud.cs.pub.ro","379","392","552","436","339","331","631"],
    ["Aurora.Luke@stud.cs.pub.ro","337","492","465","421","465","450","293"],
    ["","520","503","492","469","286","329","622"],
    ["Emery.David@stud.cs.pub.ro","465","381","523","525","412","457","422"],
    ["Lila.Matthew@stud.cs.pub.ro","235","304","171","394","428","357","357"],
    ["Jackson.Riley@stud.cs.pub.ro","353","433","465","420","503","401","354"],
    ["David.Emery@stud.cs.pub.ro","457","408","401","433","506","213","421"],
    ["Bella.Nolan@stud.cs.pub.ro","318","334","361","115","368","527","412"],
    ["Isabelle.Xavier@stud.cs.pub.ro","357","619","441","350","499","556","483"],
    ["Adalyn.Josiah@stud.cs.pub.ro","471","471","388","411","440","421","412"],
    ["London.Damian@stud.cs.pub.ro","424","226","353","443","353","237","555"],
    ["Raelynn.Anthony@stud.cs.pub.ro","245","384","323","366","503","543","525"],
    ["Savannah.Amir@stud.cs.pub.ro","459","523","62","58","369","459","469"],
    ["Kai.Violet@stud.cs.pub.ro","499","74","323","489","253","530","573"],
    ["Ezekiel.Natalie@stud.cs.pub.ro","406","331","318","405","421","411","522"],
    ["Lily.Luca@stud.cs.pub.ro","468","322","630","447","106","515","421"],
    ["Easton.Liliana@stud.cs.pub.ro","461","442","469","472","368","388","336"],
    ["Luke.Aurora@stud.cs.pub.ro","722","451","602","611","422","353","542"],
    ["Gabriella.Dominic@stud.cs.pub.ro","460","443","59","325","681","455","527"],
    ["Anthony.Raelynn@stud.cs.pub.ro","347","478","341","469","504","505","472"],
    ["Makayla.Jonathan@stud.cs.pub.ro","354","412","323","471","451","463","171"],
    ["Sebastian.Mila@stud.cs.pub.ro","506","531","237","573","388","384","451"],
    ["","126","59","480","428","433","436","486"],
    ["Nathan.Elizabeth@stud.cs.pub.ro","390","402","450","414","485","250","458"],
    ["Alaina.Zion@stud.cs.pub.ro","499","388","115","469","327","502","441"],
    ["Naomi.Aaron@stud.cs.pub.ro","235","435","527","475","293","123","508"],
    ["Liliana.Easton@stud.cs.pub.ro","250","555","361","426","700","381","603"],
    ["Gabriel.Emily@stud.cs.pub.ro","514","370","529","137","425","424","520"],
    ["Julian.Ellie@stud.cs.pub.ro","520","433","421","722","469","331","511"],
    ["Samuel.Sarah@stud.cs.pub.ro","412","531","515","533","516","529","439"],
    ["Connor.Victoria@stud.cs.pub.ro","370","253","436","502","219","452","398"],
    ["Landon.Skyler@stud.cs.pub.ro","274","424","428","262","507","446","508"],
    ["Aubrey.Owen@stud.cs.pub.ro","384","490","409","520","535","310","415"],
    ["","459","442","750","369","477","368","436"],
    ["Anna.Hudson@stud.cs.pub.ro","611","230","405","440","426","492","439"],
    ["Aria.Lucas@stud.cs.pub.ro","62","483","530","590","478","439","441"],
    ["Maria.Axel@stud.cs.pub.ro","382","429","349","331","433","531","496"],
    ["Charlotte.Jayden@stud.cs.pub.ro","443","515","366","455","59","478","505"],
    ["Nevaeh.Jordyn@stud.cs.pub.ro","366","312","424","115","508","547","506"],
    ["Sarah.Samuel@stud.cs.pub.ro","273","381","259","520","384","438","471"],
    ["Ellie.Julian@stud.cs.pub.ro","474","427","404","527","416","522","538"],
    ["Christian.Lelani@stud.cs.pub.ro","400","513","433","454","525","461","750"],
    ["","377","722","333","421","388","429","237"],
    ["Isaiah.Madelyn@stud.cs.pub.ro","503","331","467","414","531","568","259"],
    ["Eliana.Ethan@stud.cs.pub.ro","442","354","235","286","421","261","152"],
    ["Matthew.Lila@stud.cs.pub.ro","412","293","400","235","750","594","466"],
    ["Grayson.Isabella@stud.cs.pub.ro","459","520","456","74","498","515","213"],
    ["Reagan.Rowan@stud.cs.pub.ro","453","357","446","453","508","98","363"],
    ["Hudson.Anna@stud.cs.pub.ro","312","404","414","394","334","461","361"],
    ["Allison.Max@stud.cs.pub.ro","439","419","439","384","363","452","384"],
    ["Kaylee.Zane@stud.cs.pub.ro","414","644","483","513","391","312","451"],
    ["Aiden.Emma@stud.cs.pub.ro","230","370","475","722","261","442","528"],
    ["Jameson.Melanie@stud.cs.pub.ro","438","478","123","443","477","388","522"],
    ["Amelia.Caden@stud.cs.pub.ro","277","412","322","513","421","398","381"],
    ["Avery.Benjamin@stud.cs.pub.ro","388","295","441","461","591","119","553"],
    ["Jasmine.Elliot@stud.cs.pub.ro","528","391","611","341","457","502","298"],
    ["Natalie.Ezekiel@stud.cs.pub.ro","515","418","343","498","361","722","421"],
    ["Zoe.Mason@stud.cs.pub.ro","310","237","545","444","516","425","439"],
    ["Charlie.Henry@stud.cs.pub.ro","658","426","171","523","467","405","292"],
    ["Hannah.Carson@stud.cs.pub.ro","322","58","535","549","441","535","357"],
    ["Lillian.Christopher@stud.cs.pub.ro","295","332","370","419","353","408","412"],
    ["Paisley.Ezra@stud.cs.pub.ro","360","409","545","447","541","453","322"],
    ["Jack.Leah@stud.cs.pub.ro","441","412","527","441","525","322","523"],
    ["Willow.Roman@stud.cs.pub.ro","334","361","591","465","432","364","384"],
    ["Mackenzie.John@stud.cs.pub.ro","451","353","622","364","490","455","453"],
    ["Daniel.Chloe@stud.cs.pub.ro","370","277","511","497","424","404","467"],
    ["Evelyn.Logan@stud.cs.pub.ro","513","363","431","446","350","523","522"],
    ["Riley.Jackson@stud.cs.pub.ro","421","523","441","230","286","523","388"],
    ["Benjamin.Avery@stud.cs.pub.ro","465","381","450","405","59","341","644"],
    ["","527","357","545","485","59","332","549"],
    ["Colton.Ariana@stud.cs.pub.ro","440","498","525","370","451","322","340"],
    ["Caden.Amelia@stud.cs.pub.ro","477","322","421","630","467","79","498"],
    ["Callie.Adam@stud.cs.pub.ro","442","357","451","503","477","525","775"],
    ["Ethan.Eliana@stud.cs.pub.ro","722","442","619","472","123","296","520"],
    ["Cameron.Peyton@stud.cs.pub.ro","338","419","391","556","775","478","503"],
    ["Jacob.Scarlett@stud.cs.pub.ro","541","796","467","292","432","213","440"],
    ["Gianna.Levi@stud.cs.pub.ro","565","286","82","355","538","529","479"],
    ["Aaliyah.Oliver@stud.cs.pub.ro","402","404","58","119","529","498","543"],
    ["Nicholas.Hazel@stud.cs.pub.ro","432","478","451","322","432","529","603"],
    ["Emilia.Isaac@stud.cs.pub.ro","507","631","520","237","274","405","492"],
    ["Eleanor.Adrian@stud.cs.pub.ro","454","398","374","304","478","497","472"],
    ["Madelyn.Isaiah@stud.cs.pub.ro","126","327","446","483","405","421","523"],
    ["Anaya.Malachi@stud.cs.pub.ro","506","357","552","298","351","474","291"],
    ["Penelope.Maverick@stud.cs.pub.ro","312","396","520","467","124","555","658"],
    ["Maya.Asher@stud.cs.pub.ro","333","496","476","514","123","555","357"],
    ["Peyton.Cameron@stud.cs.pub.ro","750","750","401","461","414","692","277"],
    ["Ava.Elijah@stud.cs.pub.ro","226","383","82","332","277","412","392"],
    ["Amaya.Hunter@stud.cs.pub.ro","250","430","523","508","439","399","542"],
    ["Chloe.Daniel@stud.cs.pub.ro","327","525","245","398","291","499","590"],
    ["Mia.Mateo@stud.cs.pub.ro","465","555","722","368","237","465","423"]]
