package xph_covid19.data is

   u_range : unknowns_range :=
     (a1 => (1.0e-10, 1.0),
      b1 => (1.0e-10, 3.0),
      b2 => (1.0e-10, 1.0),
      k1 => (1.0e-10, 1.0),
      k2 => (1.0e-10, 5.0));

   all_countries : constant countries :=
     (("Aruba                           ", 180.0, 105845.0, 588.0),
      ("Afghanistan                     ", 652860.0, 37172386.0, 56.9),
      ("Angola                          ", 1246700.0, 30809762.0, 24.7),
      ("Albania                         ", 27400.0, 2866376.0, 104.6),
      ("Andorra                         ", 470.0, 77006.0, 163.8),
      ("United Arab Emirates            ", 71020.0, 9630959.0, 135.6),
      ("Argentina                       ", 2736690.0, 44494502.0, 16.3),
      ("Armenia                         ", 28470.0, 2951776.0, 103.7),
      ("Antigua and Barbuda             ", 440.0, 96286.0, 218.8),
      ("Australia                       ", 7692020.0, 24992369.0, 3.2),
      ("Austria                         ", 82523.0, 8847037.0, 107.2),
      ("Azerbaijan                      ", 82670.0, 9942334.0, 120.3),
      ("Belgium                         ", 30280.0, 11422068.0, 377.2),
      ("Benin                           ", 112760.0, 11485048.0, 101.9),
      ("Burkina Faso                    ", 273600.0, 19751535.0, 72.2),
      ("Bangladesh                      ", 130170.0, 161356039.0, 1239.6),
      ("Bulgaria                        ", 108560.0, 7024216.0, 64.7),
      ("Bahrain                         ", 778.0, 1569439.0, 2017.3),
      ("Bahamas                         ", 10010.0, 385640.0, 38.5),
      ("Bosnia and Herzegovina          ", 51200.0, 3323929.0, 64.9),
      ("Belarus                         ", 202988.0, 9485386.0, 46.7),
      ("Belize                          ", 22810.0, 383071.0, 16.8),
      ("Bermuda                         ", 54.0, 63968.0, 1184.6),
      ("Bolivia                         ", 1083300.0, 11353142.0, 10.5),
      ("Brazil                          ", 8358140.0, 209469333.0, 25.1),
      ("Barbados                        ", 430.0, 286641.0, 666.6),
      ("Brunei Darussalam               ", 5270.0, 428962.0, 81.4),
      ("Bhutan                          ", 38144.0, 754394.0, 19.8),
      ("Central African Republic        ", 622980.0, 4666377.0, 7.5),
      ("Canada                          ", 9093510.0, 37058856.0, 4.1),
      ("Switzerland                     ", 39516.0, 8516543.0, 215.5),
      ("Chile                           ", 743532.0, 18729160.0, 25.2),
      ("China                           ", 9388210.0, 1392730000.0, 148.3),
      ("Cote dIvoire                    ", 318000.0, 25069229.0, 78.8),
      ("Cameroon                        ", 472710.0, 25216237.0, 53.3),
      ("Democratic Republic of the Congo", 2267050.0, 84068091.0, 37.1),
      ("Congo                           ", 341500.0, 5244363.0, 15.4),
      ("Colombia                        ", 1109500.0, 49648685.0, 44.7),
      ("Cape Verde                      ", 4030.0, 543767.0, 134.9),
      ("Costa Rica                      ", 51060.0, 4999441.0, 97.9),
      ("Cuba                            ", 104020.0, 11338138.0, 109.0),
      ("Curacao                         ", 444.0, 159849.0, 360.0),
      ("Cayman Islands                  ", 240.0, 64174.0, 267.4),
      ("Cyprus                          ", 9240.0, 1189265.0, 128.7),
      ("Czech Republic                  ", 77220.0, 10625695.0, 137.6),
      ("Germany                         ", 349360.0, 82927922.0, 237.4),
      ("Djibouti                        ", 23180.0, 958920.0, 41.4),
      ("Dominica                        ", 750.0, 71625.0, 95.5),
      ("Denmark                         ", 41990.0, 5797446.0, 138.1),
      ("Dominican Republic              ", 48310.0, 10627165.0, 220.0),
      ("Algeria                         ", 2381740.0, 42228429.0, 17.7),
      ("Ecuador                         ", 248360.0, 17084357.0, 68.8),
      ("Egypt                           ", 995450.0, 98423595.0, 98.9),
      ("Spain                           ", 499564.0, 46723749.0, 93.5),
      ("Estonia                         ", 43470.0, 1320884.0, 30.4),
      ("Ethiopia                        ", 1000000.0, 109224559.0, 109.2),
      ("Finland                         ", 303910.0, 5518050.0, 18.2),
      ("Fiji                            ", 18270.0, 883483.0, 48.4),
      ("France                          ", 547557.0, 66987244.0, 122.3),
      ("Faroe Islands                   ", 1396.0, 48497.0, 34.7),
      ("Gabon                           ", 257670.0, 2119275.0, 8.2),
      ("United Kingdom                  ", 241930.0, 66488991.0, 274.8),
      ("Georgia                         ", 69490.0, 3731000.0, 53.7),
      ("Ghana                           ", 227540.0, 29767108.0, 130.8),
      ("Gibraltar                       ", 10.0, 33718.0, 3371.8),
      ("Guinea                          ", 245720.0, 12414318.0, 50.5),
      ("Gambia                          ", 10120.0, 2280102.0, 225.3),
      ("Guinea Bissau                   ", 36125.0, 1920917.0, 53.1),
      ("Equatorial Guinea               ", 28050.0, 1308974.0, 46.7),
      ("Greece                          ", 128900.0, 10727668.0, 83.2),
      ("Grenada                         ", 340.0, 111454.0, 327.8),
      ("Greenland                       ", 410450.0, 56025.0, 0.1),
      ("Guatemala                       ", 107160.0, 17247807.0, 161.0),
      ("Guam                            ", 540.0, 165768.0, 307.0),
      ("Guyana                          ", 196850.0, 779004.0, 4.0),
      ("Honduras                        ", 111890.0, 9587522.0, 85.7),
      ("Croatia                         ", 55960.0, 4089400.0, 73.1),
      ("Haiti                           ", 27560.0, 11123176.0, 403.6),
      ("Hungary                         ", 90530.0, 9768785.0, 107.9),
      ("Indonesia                       ", 1811570.0, 267663435.0, 147.8),
      ("Isle of Man                     ", 570.0, 84077.0, 147.5),
      ("India                           ", 2973190.0, 1352617328.0, 454.9),
      ("Ireland                         ", 68890.0, 4853506.0, 70.5),
      ("Iran                            ", 1628760.0, 81800269.0, 50.2),
      ("Iraq                            ", 434128.0, 38433600.0, 88.5),
      ("Iceland                         ", 100250.0, 353574.0, 3.5),
      ("Israel                          ", 21640.0, 8883800.0, 410.5),
      ("Italy                           ", 294140.0, 60431283.0, 205.5),
      ("Jamaica                         ", 10830.0, 2934855.0, 271.0),
      ("Jordan                          ", 88780.0, 9956011.0, 112.1),
      ("Japan                           ", 364560.0, 126529100.0, 347.1),
      ("Kazakhstan                      ", 2699700.0, 18276499.0, 6.8),
      ("Kenya                           ", 569140.0, 51393010.0, 90.3),
      ("Kyrgyzstan                      ", 191800.0, 6315800.0, 32.9),
      ("Cambodia                        ", 176520.0, 16249798.0, 92.1),
      ("Saint Kitts and Nevis           ", 260.0, 52441.0, 201.7),
      ("South Korea                     ", 97489.0, 51635256.0, 529.7),
      ("Kuwait                          ", 17820.0, 4137309.0, 232.2),
      ("Lebanon                         ", 10230.0, 6848925.0, 669.5),
      ("Liberia                         ", 96320.0, 4818977.0, 50.0),
      ("Libya                           ", 1759540.0, 6678567.0, 3.8),
      ("Saint Lucia                     ", 610.0, 181889.0, 298.2),
      ("Liechtenstein                   ", 160.0, 37910.0, 236.9),
      ("Sri Lanka                       ", 62710.0, 21670000.0, 345.6),
      ("Lithuania                       ", 62642.0, 2789533.0, 44.5),
      ("Luxembourg                      ", 2430.0, 607728.0, 250.1),
      ("Latvia                          ", 62180.0, 1926542.0, 31.0),
      ("Morocco                         ", 446300.0, 36029138.0, 80.7),
      ("Moldova                         ", 32890.0, 3545883.0, 107.8),
      ("Madagascar                      ", 581800.0, 26262368.0, 45.1),
      ("Maldives                        ", 300.0, 515696.0, 1719.0),
      ("Mexico                          ", 1943950.0, 126190788.0, 64.9),
      ("North Macedonia                 ", 25220.0, 2082958.0, 82.6),
      ("Mali                            ", 1220190.0, 19077690.0, 15.6),
      ("Malta                           ", 320.0, 483530.0, 1511.0),
      ("Myanmar                         ", 653080.0, 53708395.0, 82.2),
      ("Montenegro                      ", 13450.0, 622345.0, 46.3),
      ("Mongolia                        ", 1553560.0, 3170208.0, 2.0),
      ("Mozambique                      ", 786380.0, 29495962.0, 37.5),
      ("Mauritania                      ", 1030700.0, 4403319.0, 4.3),
      ("Mauritius                       ", 2030.0, 1265303.0, 623.3),
      ("Malaysia                        ", 328550.0, 31528585.0, 96.0),
      ("Namibia                         ", 823290.0, 2448255.0, 3.0),
      ("New Caledonia                   ", 18280.0, 284060.0, 15.5),
      ("Niger                           ", 1266700.0, 22442948.0, 17.7),
      ("Nigeria                         ", 910770.0, 195874740.0, 215.1),
      ("Nicaragua                       ", 120340.0, 6465513.0, 53.7),
      ("Netherlands                     ", 33690.0, 17231017.0, 511.5),
      ("Norway                          ", 365123.0, 5314336.0, 14.6),
      ("Nepal                           ", 143350.0, 28087871.0, 195.9),
      ("New Zealand                     ", 263310.0, 4885500.0, 18.6),
      ("Oman                            ", 309500.0, 4829483.0, 15.6),
      ("Pakistan                        ", 770880.0, 212215030.0, 275.3),
      ("Panama                          ", 74340.0, 4176873.0, 56.2),
      ("Peru                            ", 1280000.0, 31989256.0, 25.0),
      ("Philippines                     ", 298170.0, 106651922.0, 357.7),
      ("Papua New Guinea                ", 452860.0, 8606316.0, 19.0),
      ("Poland                          ", 306190.0, 37978548.0, 124.0),
      ("Portugal                        ", 91605.6, 10281762.0, 112.2),
      ("Paraguay                        ", 397300.0, 6956071.0, 17.5),
      ("Palestine                       ", 6020.0, 4569087.0, 759.0),
      ("French Polynesia                ", 3660.0, 277679.0, 75.9),
      ("Qatar                           ", 11610.0, 2781677.0, 239.6),
      ("Romania                         ", 230080.0, 19473936.0, 84.6),
      ("Russia                          ", 16376870.0, 144478050.0, 8.8),
      ("Rwanda                          ", 24670.0, 12301939.0, 498.7),
      ("Saudi Arabia                    ", 2149690.0, 33699947.0, 15.7),
      ("Sudan                           ", 1886000.0, 41801533.0, 22.2),
      ("Senegal                         ", 192530.0, 15854360.0, 82.3),
      ("Singapore                       ", 709.0, 5638676.0, 7953.0),
      ("El Salvador                     ", 20720.0, 6420744.0, 309.9),
      ("San Marino                      ", 60.0, 33785.0, 563.1),
      ("Somalia                         ", 627340.0, 15008154.0, 23.9),
      ("Serbia                          ", 87460.0, 6982084.0, 79.8),
      ("Suriname                        ", 156000.0, 575991.0, 3.7),
      ("Slovakia                        ", 48080.0, 5447011.0, 113.3),
      ("Slovenia                        ", 20142.0, 2067372.0, 102.6),
      ("Sweden                          ", 407310.0, 10183175.0, 25.0),
      ("Seychelles                      ", 460.0, 96762.0, 210.4),
      ("Syria                           ", 183630.0, 16906283.0, 92.1),
      ("Chad                            ", 1259200.0, 15477751.0, 12.3),
      ("Togo                            ", 54390.0, 7889094.0, 145.0),
      ("Thailand                        ", 510890.0, 69428524.0, 135.9),
      ("Timor Leste                     ", 14870.0, 1267972.0, 85.3),
      ("Trinidad and Tobago             ", 5130.0, 1389858.0, 270.9),
      ("Tunisia                         ", 155360.0, 11565204.0, 74.4),
      ("Turkey                          ", 769630.0, 82319724.0, 107.0),
      ("United Republic of Tanzania     ", 885800.0, 56318348.0, 63.6),
      ("Uganda                          ", 200520.0, 42723139.0, 213.1),
      ("Ukraine                         ", 579290.0, 44622516.0, 77.0),
      ("Uruguay                         ", 175020.0, 3449299.0, 19.7),
      ("United States of America        ", 9147420.0, 327167434.0, 35.8),
      ("Uzbekistan                      ", 425400.0, 32955400.0, 77.5),
      ("Saint Vincent and the Grenadines", 390.0, 110210.0, 282.6),
      ("Venezuela                       ", 882050.0, 28870195.0, 32.7),
      ("United States Virgin Islands    ", 350.0, 106977.0, 305.6),
      ("Vietnam                         ", 310070.0, 95540395.0, 308.1),
      ("Kosovo                          ", 10887.0, 1845300.0, 169.5),
      ("South Africa                    ", 1213090.0, 57779622.0, 47.6),
      ("Zambia                          ", 743390.0, 17351822.0, 23.3),
      ("Zimbabwe                        ", 386850.0, 14439018.0, 37.3),
      ("Puerto Rico                     ", 13800.0, 2933404.0, 212.6),
      ("Sierra Leone                    ", 71740.0, 7813207.0, 108.9),
      ("South Sudan                     ", 619745.0, 11062114.0, 17.8),
      ("Tajikistan                      ", 143100.0, 9321023.0, 65.1),
      ("Diamond Princess Liner          ", 0.14, 3711.0, 26507.0));


end xph_covid19.data;
