
De wereld is overgenomen door robots, maar jij bent kunnen ontsnappen door je te verstoppen in de kast. Baan je een weg naar de ruimtebasis van de gemene Professor die de robots bestuurd, om de wereld te redden.

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
ROBOT: GEGROET. BIEP. BOEP.... SNIK... OH MIJN ARME KAT.

- (keuzes)
    * Hoe komt het dat jij hier op de bank zit? [] Niet veel te doen vandaag?
      ROBOT: DIT RIOOLDEKSEL MOET BEWAAKT WORDEN. BIEP. BOEP.
      SPELER: Ach zo, dat moet wel een belangrijke taak zijn. Euh... biep. boep.
      ROBOT: JAZEKER. TUNNEL NAAR LANCEERBASIS! BIEEEP.
      -> keuzes
    * (kat)Je ziet er niet erg vrolijk uit waarde kollega... Wat ligt er op je transistors?
      ROBOT: MIJN KAT. MIJN ARME KAT. ZE IS VERDWENEN.
      -> keuzes
    + {kat} Zal ik je kat helpen vinden?
      ROBOT: DAT ZOU FANTASTISCH ZIJN.
      ROBOT: MISSCHIEN DAT ZE TERUGKOMT ALS IK HAAR ETEN KAN GEVEN.
      -> keuzes
    + Fijne dag nog! [] Veel sucess bij het vinden van je kat! 
      -> END

=== winkel ===

WINKELIER: WAARMEE KAN IK U VAN DIENST ZIJN?
WINKELIER: IK HEB VIJZEN, MOTOROLIE, EN VERSE TRANSISTORS.

- (keuzes)
    * {robot_op_bank.kat} Ik zoek katteneten
      WINKELIER: KATTENETEN? WAAR HEB JE DAT VOOR NODIG
      SPELER: Voor mijn kat, tiens.
      WINKELIER: LOGICA! DE VORIGE UITBATER HEEFT NOG KATTENETEN ACHTER GELATEN. ALSJEBLIEFT!
      >>> krijg katteneten
      -> END
    + Ik kijk alleen maar
      -> END

=== item_is_te_ver ===

{~Daar kan ik niet aan|Misschien als ik iets dichter ga staan|Dat is wel erg ver|Zo gaat dat niet lukken}

->END

