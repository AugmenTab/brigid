#! python3

from pathlib import Path
from re import split, sub, UNICODE
from string import capwords, punctuation
from urllib.error import HTTPError
from urllib.request import Request, urlopen

from bs4 import BeautifulSoup


def indent(n):
    return " " * n


def create_file(entities):
    module_comment = '''-- | This module exposes a number of convenience functions to write an HTML
-- entity. Each entity is written using the HTML decimal code. The names for
-- these entity constants were derived from a description of the entity's
-- printed appearance or function as a control character. They are ordered by
-- their HTML decimal code, ascending.
--
-- A complete list of the codes and their details (from which this module was
-- assembled) can be found here:
--
-- https://www.ee.ucl.ac.uk/mflanaga/java/HTMLandASCIItableC1.html
--
'''

    open(str(Path.cwd()) + '/src/Brigid/HTML/Entities.hs', 'w').close()

    with open(str(Path.cwd()) + '/src/Brigid/HTML/Entities.hs', 'a') as f:
        def write_import(prepend, name):
            f.write("\n" + indent(2) + prepend + " " + name)


        f.write(module_comment)
        f.write("module Brigid.HTML.Entities")
        write_import("(", entities[0]['name'])

        for entity in entities[1:]:
            write_import(",", entity['name'])

        f.write("\n" + indent(2) + ") where\n")
        f.write("\n" + "import Brigid.HTML.Elements.Children (ValidChild)")
        f.write("\n" + "import Brigid.HTML.Elements.Internal (ChildHTML (Tag_Entity))")
        f.write("\n" + "import Brigid.HTML.Elements.Tags (Text)")

        for entity in entities:
            f.write("\n\n" + "-- | The " + entity['comment'] + " HTML entity")
            if entity['symbol'] == "":
                f.write('.')
            else:
                f.write(" ('" + entity['symbol'] + "').")
            f.write("\n" + entity['name'] + " :: ValidChild Text parent grandparent")
            f.write("\n" + indent(len(entity['name'])) + " => ChildHTML parent grandparent")
            f.write("\n" + entity['name'] + " = Tag_Entity \"" + entity['code'] + "\"")


def sanitize(input_string):
    pattern = r'[\s\u200B\u00A0\u202F\u205F\u3000\u200C\u200D\u200E\u200F\u2060\u2061\u2062\u2063]'
    return sub(pattern, '', input_string, flags=UNICODE)


def lower_camel_case(words):
    camel_cased = words[0].lower()

    for word in words[1:]:
        if word == "-":
            continue
        elif any(char in punctuation for char in word):
            break
        else:
            camel_cased += capwords(word)

    return camel_cased


def parse_entities(rows):
    entities = {}

    for row in rows:
        cells = row.find_all('td')
        comment = cells[6].contents[0]
        name = lower_camel_case(split(r'\s+|(?<=\w)-(?=\w)', comment))

        entity = {
            'symbol': sanitize(cells[0].contents[0]),
            'code': cells[3].contents[0],
            'name': name,
            'comment': comment
        }

        if entity['code'] not in entities:
            entities[entity['code']] = entity

    return list(entities.values())


def get_table_rows(res):
    return res.find('table', {}).find_all('tr')[1:]


def get_soup(url):
    req = Request(url)

    req.add_header('User-Agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0')
    req.add_header('Accept', 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8')
    req.add_header('Accept-Language', 'en-US,en;q=0.5')

    with urlopen(req) as response:
        return BeautifulSoup(response.read().decode('utf-8'), 'html.parser')


def err_out():
    print("Exiting...")
    exit(1)


def validate_url(url):
    if not validators.url(url):
        print("Invalid URL: " + url)
        err_out()


if __name__ == "__main__":
    try:
        print("Fetching entities...")
        soup = get_soup("https://www.ee.ucl.ac.uk/mflanaga/java/HTMLandASCIItableC1.html")
        print("Entities fetched.")

        print("Parsing entities...")
        entities = parse_entities(get_table_rows(soup))
        print("Entities parsed..")

        print("Writing Brigid.HTML.Entities...")
        create_file(entities)
        print("Done.")
        exit(0)

    except HTTPError as e:
        print("HTTPError " + str(e.code) + ": " + e.reason)
        err_out()
