
db_open :- odbc_connect('prechacthis', _, [alias(db)]).
db_close :- odbc_disconnect(db).

db_create_tables :-
	odbc_query(db,
		'CREATE TABLE ...\
		) ENGINE = innodb;'
	).


db_siteswap_exists(Siteswap) :-   %should return stars
	format(string(SQL),
			'SELECT `siteswap` \
			FROM `siteswaps` \
			WHERE `siteswap` LIKE "~w" \
			LIMIT 1',
			[Siteswap]
		),
		odbc_query(db, SQL, row(_)).

/*
%% [prechacthis]
%% DSN = prechacthis
%% Driver = MySQL
%% SERVER = localhost
%% USER = jj
%% PASSWORD = dbpw42
%% DATABASE = prechacthis



%%% old %%%

db_create_table :-
	odbc_query(db,
		'CREATE TABLE `siteswaps` ( \
			`id` INT UNSIGNED NOT NULL AUTO_INCREMENT , \
			`siteswap` VARCHAR( 100 ) NOT NULL , \
			`period` SMALLINT UNSIGNED NOT NULL , \
			`objects` SMALLINT UNSIGNED NOT NULL , \
			`maxheight` SMALLINT UNSIGNED NOT NULL , \ 
			PRIMARY KEY ( `id` ) , \
			INDEX ( `siteswap` ) , \
			INDEX ( `period` , `objects` , `maxheight` ) \
		) ENGINE = innodb;'
	),
	odbc_query(db,
		'CREATE TABLE `throws` (
			`siteswap_id` INT UNSIGNED NOT NULL ,
			`position` SMALLINT UNSIGNED NOT NULL ,
			`throw` SMALLINT UNSIGNED NOT NULL ,
			PRIMARY KEY ( `siteswap_id` , `position` )
		) ENGINE = innodb;'
	).

db_check_table :- true,!.
db_check_table :-
	odbc_query(db_check,'SELECT `TABLE_NAME` FROM `TABLES` WHERE `TABLE_SCHEMA` LIKE "prechacthis" AND `TABLE_NAME` LIKE "siteswaps"',_),
	odbc_query(db_check,'SELECT `TABLE_NAME` FROM `TABLES` WHERE `TABLE_SCHEMA` LIKE "prechacthis" AND `TABLE_NAME` LIKE "throws"',_).
		
db_HardcodeSiteswaps(ObjectMax,PeriodMax,MaxHeight) :-
	db_HardcodeSiteswaps(1,ObjectMax,1,PeriodMax,MaxHeight).
db_HardcodeSiteswaps(ObjectMin,ObjectMax,PeriodMin,PeriodMax,MaxHeight) :-
	db_open,
	%(db_check_table; db_create_table),
	forall(
		(
			between(PeriodMin,PeriodMax,Period),
			between(ObjectMin,ObjectMax,Objects)
		),(
			format('objects: ~w, period: ~w\n', [Objects,Period]),
			db_WriteSiteswaps(Objects,Period,MaxHeight)
		)
	),
	db_close.

db_WriteSiteswaps(Objects, Period, MaxHeight) :-
	forall(
		(
			length(Siteswap, Period),
			siteswap(Objects, Siteswap, MaxHeight),
			maxHeight(Siteswap, Height)
		),(
			db_WriteSingleSiteswap(Siteswap,Period,Objects,Height)
		)
	).
	

db_WriteSingleSiteswap(Siteswap,_,_,_) :- db_RotationInDB(Siteswap),!.
db_WriteSingleSiteswap(Siteswap,Period,Objects,Height) :-
	%% write siteswap information
	format(string(SQL1),
		'INSERT INTO `siteswaps` ( `siteswap` , `period` , `objects` , `maxheight` ) \
		VALUES ("~w", "~d", "~d", "~d");',
		[Siteswap,Period,Objects,Height]
	),
	odbc_query(db,SQL1),
	%% get siteswap id
	format(string(SQL2),
		'SELECT `id` \
		FROM `siteswaps` \
		WHERE `siteswap` LIKE "~w" \
		LIMIT 1',
		[Siteswap]
	),
	odbc_query(db,SQL2,row(ID)),
	
	forall(between(1,Period,Position),
		(
			nth1(Position, Siteswap, Throw),
			format(string(SQL),
				'INSERT INTO `throws` ( `siteswap_id` , `position` , `throw` ) \
					VALUES ("~d", "~d", "~d");',
				[ID,Position,Throw]
			),
			odbc_query(db,SQL)
		)
	).
			
	
db_RotationInDB(Siteswap) :- not(db_NoRotationInDB(Siteswap)).
db_NoRotationInDB(Siteswap) :-
	forall(
		(
			rotate(Siteswap,Rotation)
		),(
			format(string(SQL),
				'SELECT `period` \
				FROM `siteswaps` \
				WHERE `siteswap` LIKE "~w" \
				LIMIT 1',
				[Rotation]
			),
			not(odbc_query(db,SQL,row(_)))
		)
	).	


db_siteswap(Siteswap,Period,Objects,MaxHeight) :-
	number(Period),
	number(Objects),
	number(MaxHeight),
	db_siteswapID(ID,Period,Objects,MaxHeight),
	db_siteswap_by_id(Siteswap,ID).
	
db_siteswapID(ID,Period,Objects,MaxHeight) :-
	format(atom(SQL),
		'SELECT `id` FROM `siteswaps` WHERE `period` = "~d" AND `objects` = "~d" AND `maxheight` <= "~d";',
		[Period,Objects,MaxHeight]
	),
	odbc_query(db,SQL,row(ID)).
	
db_siteswap_by_id(Siteswap,ID) :-		
	format(atom(SQL),
		'SELECT `throw` FROM `throws` WHERE `siteswap_id` = "~d" ORDER BY `position` ASC;',
		[ID]
	),
	odbc_query(db,SQL,Siteswap,[findall(Throw,row(Throw))]).
*/
