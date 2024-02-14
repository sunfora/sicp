CREATE TABLE IF NOT EXISTS employees (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  salary INTEGER NOT NULL,
  address TEXT,
  phone TEXT
);

INSERT INTO employees (name, salary, address, phone)
VALUES
("Gayle Barnes",      124000, "Dedham, Massachusetts(MA)",      "(314) 725-9276"),
("James Blair",       50040,  "Webster, Massachusetts(MA)",     "(207) 646-7122"),
("Christine Booth",   73414,  "Stoughton, Massachusetts(MA)",   "(781) 436-5875"),
("Susan Bridge",      42414,  "Fall River, Massachusetts(MA)",  "(508) 567-3653"),
("Andrew Bridson",    124124, "Northbridge, Massachusetts(MA)", "(860) 963-0492"),
("Graham Carroll",    42144,  "Franklin, Massachusetts(MA)",    "(817) 276-3240"),
("Belayet Choudhury", 76324,  "Boston, Massachusetts(MA)",      "(617) 541-8336"),
("Phillip Collings",  39486,  "Waltham, Massachusetts(MA)",     "(781) 893-9908"),
("Graham Cooper",     23058,  "Whately, Massachusetts(MA)",     "(615) 228-2158"),
("Glen Dale",         48097,  "Haverhill, Massachusetts(MA)",   "(603) 382-2058"),
("Christian Davies",  58723,  "Roslindale, Massachusetts(MA)",  "(757) 531-9199");

