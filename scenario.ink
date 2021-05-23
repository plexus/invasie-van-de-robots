VAR inventory_katteneten = false

-> END

=== intro ===

ROBOT: HIER IS NIEMAND MEER. TIJD OM VERSLAG UIT TE BRENGEN BIJ DE BAAS.
ANDERE ROBOT: GOED WERK! ALLE VLEESZAKKEN ZITTEN GEVANGEN. DE AARDE IS NU VAN DE ROBOTS!
ROBOT: MOCHT ER TOCH NOG IEMAND OPDUIKEN, DAN NEMEN WE DIE DAN WEL TE GRAZEN. HAHAHAHAHA
>>> robots verlaten kamer
>>> kast gaat open
SPELER: Oef, ze hebben me niet gezien, maar wat nu?
SPELER: Ik moet mijn vrienden redden! Maar eerst moet ik veilig het huis uit kunnen komen.

-> END

=== slaapkamer ===

Voor dat je naar buiten kan heb je een vermomming nodig. Verkleed je in een robot zodat je niet opvalt buiten.

-> END

=== robot_op_bank ===

SPELER: Hallo kollega!
{not robot_krijgt_katteneten and not kat_zoeken: ROBOT: GEGROET. BIEP. BOEP.... SNIK... OH MIJN ARME KAT.}
{kat_zoeken: ROBOT: Heb je al katteneten gevonden?}
{robot_krijgt_katteneten: "ROBOT: HALLO KOLLEGA! NOG EENS BEDANKT VOOR HET KATTENETEN!"}

- (keuzes)
    * Hoe komt het dat jij hier op de bank zit? [] Niet veel te doen vandaag?
      ROBOT: DIT RIOOLDEKSEL MOET BEWAAKT WORDEN. BIEP. BOEP.
      SPELER: Ach zo, dat moet wel een belangrijke taak zijn. Euh... biep. boep.
      ROBOT: JAZEKER. TUNNEL NAAR LANCEERBASIS! BIEEEP.
      -> keuzes
    * (kat) {not robot_krijgt_katteneten} Je ziet er niet erg vrolijk uit waarde kollega... Wat ligt er op je transistors?
      ROBOT: MIJN KAT. MIJN ARME KAT. ZE IS VERDWENEN.
      -> keuzes
    * (kat_zoeken) {not robot_krijgt_katteneten} {kat} Zal ik je kat helpen vinden?
      ROBOT: DAT ZOU FANTASTISCH ZIJN.
      ROBOT: MISSCHIEN DAT ZE TERUGKOMT ALS IK HAAR ETEN KAN GEVEN.
      ROBOT: BIEP. BOEP.
      -> keuzes
    * {inventory_katteneten} Hier is je katteneten!
      -> robot_krijgt_katteneten 
    + Fijne dag nog! [] Veel sucess bij het vinden van je kat! 
      -> END

=== robot_krijgt_katteneten ===

ROBOT: KATTENVOER! NU KOMT MIJN KAT ZEKER TERUG. BIEP BOEP JEEJ!
ROBOT: HEEL ERG BEDANKT. KAN IK JE MISSCHIEN ERGENS MEE HELPEN? ... BIEEEEP
SPELER: Je zou misschien het riooldeksel kunnen open doen.
ROBOT: RIOOLDEKSEL IS SUPER GEHEIME TUNNEL NAAR LANCEERBASIS. MOET BEWAAKT WORDEN!
SPELER: Als je het deksel even open doet, dan zal ik de binnenkant bewaken! Goed idee, toch!
ROBOT: ....
ROBOT: OK!
>>> verwijder katteneten
>>> riool gaat open

-> END

=== winkel ===

WINKELIER: WAARMEE KAN IK U VAN DIENST ZIJN?

WINKELIER: {vraag_katteneten: TOCH NIET WEER KATTENTEN?} IK HEB VIJZEN, MOTOROLIE, EN VERSE TRANSISTORS.

- (keuzes)
    * {inventory_katteneten} ....
    * (vraag_katteneten) {robot_op_bank.kat} {not robot_krijgt_katteneten} Ik zoek katteneten
      WINKELIER: KATTENETEN? WAAR HEB JE DAT VOOR NODIG
      SPELER: Voor mijn kat, tiens.
      WINKELIER: LOGICA! DE VORIGE UITBATER HEEFT NOG KATTENETEN ACHTER GELATEN. ALSJEBLIEFT!
      >>> krijg katteneten
      -> END
    + Ik kijk alleen maar
      -> END

=== item_is_te_ver ===

{~Daar kan ik niet aan|Misschien als ik iets dichter ga staan|Dat is wel erg ver}

->END

=== combinatie_ken_ik_niet ===

{~Dat snap ik niet|Hoe bedoel je?|Dat heeft geen zin|Ik denk niet dat dat gaat lukken}

->END

=== eerst_verkleden ===

Ben je gek? Het krioelt daar van de robots! Als ik zo buiten kom is het GAME OVER.
Misschien kan ik me eerst vermommen in een robot. Eens kijken wat ik hier nog liggen heb.

->END

=== eindbaas ===

SPELER: Haha, je spel is uit!
BAAS: Nee, dat kan niet... Neeeeeeeee
>>> baas valt om

->END