#! python3

from json import loads
from pathlib import Path
from re import split
from urllib.request import Request, urlopen


def indent(n):
    return " " * n


def create_module(mime_types):
    module_comment = ''' -- A complete list of the MIME types and their details (from which this module
-- was assembled) can be found here:
--
-- https://s-randomfiles.s3.amazonaws.com/mime/allMimeTypes.json
--
'''

    open(str(Path.cwd()) + '/src/Brigid/Types/MIME_Types.hs', 'w').close()

    with open(str(Path.cwd()) + '/src/Brigid/Types/MIME_Types.hs', 'a') as f:
        def write_import(spaces, prepend, name):
            f.write("\n" + indent(spaces) + prepend + " " + name)


        f.write(module_comment)
        f.write("module Brigid.Types.MIME_Types")
        write_import(2, "(", "MIME_Type")
        write_import(6, "(", mime_types[0]['constructor'])

        for mime_type in mime_types[1:]:
            write_import(6, ",", mime_type['constructor'])

        f.write("\n" + indent(6) + ")")
        write_import(2, ",", "mimeTypeToBytes")
        write_import(2, ",", "mimeTypeToText")
        write_import(2, ",", "mimeTypeExtensions")
        f.write("\n" + indent(2) + ") where")

        f.write("\nimport Data.ByteString.Lazy qualified as LBS")
        f.write("\nimport Data.Text qualified as T\n\n")

        f.write("data MIME_Type")
        write_import(2, "=", mime_types[0]['constructor'])

        for mime_type in mime_types[1:]:
            write_import(2, "|", mime_type['constructor'])

        f.write("\n" + indent(2) + "deriving (Bounded, Enum, Eq)\n\n")

        def write_convert_function(name, type_):
            f.write("mimeTypeTo" + name + " :: MIME_Type -> " + type_ + "\n")
            f.write("mimeTypeTo" + name + " mimeType =\n")
            f.write(indent(2) + "case mimeType of\n")

            for mime_type in mime_types:
                f.write(f"{indent(4)}{mime_type['constructor']} -> \"{mime_type['text']}\"\n")

            f.write("\n")

        write_convert_function("Bytes", "LBS.ByteString")
        write_convert_function("Text", "T.Text")

        f.write("mimeTypeExtensions :: MIME_Type -> [T.Text]\n")
        f.write("mimeTypeExtensions mimeType =\n")
        f.write(indent(2) + "case mimeType of\n")

        def render_extensions(extensions):
            count = len(extensions)

            f.write("[")

            if count == 0:
                f.write("")

            elif count == 1:
                f.write(f" \"{extensions[0]}\" ")

            else:
                f.write(f" \"{extensions[0]}\"")

                for extension in extensions[1:]:
                  f.write(f", \"{extension}\"")

                f.write(" ")

            f.write("]")

        for mime_type in mime_types:
            f.write(f"{indent(4)}{mime_type['constructor']} -> ")
            render_extensions(mime_type['extensions'])
            f.write("\n")


def make_constructor(text):
    if text == "/":
        return None
    else:
        type_part, subtype_part = text.split('/')

        def clean_part(part):
            segments = split(r'[\(\)-.+]', part)
            return ''.join(segment.capitalize() for segment in segments)

        return f"{clean_part(type_part)}_{clean_part(subtype_part)}"


def parse_mime_type(mime_type):
    for key, value in mime_type.items():
        constructor = make_constructor(key)

        if constructor is not None:
            return {
                'text': key,
                'constructor': constructor,
                'extensions': value,
            }

        else:
            return None


def parse_mime_types(mime_types):
    parsed_mime_types = {}

    for mime_type in mime_types:
        parsed_mime_type = parse_mime_type(mime_type)

        if parsed_mime_type is not None:
            constructor = parsed_mime_type['constructor']

            if parsed_mime_type['constructor'] in parsed_mime_types:
                parsed_mime_types[constructor]['extensions'] += parsed_mime_type['extensions']

            else:
                parsed_mime_types[constructor] = parsed_mime_type

    return parsed_mime_types


def get_mime_types(url):
    req = Request(url)

    req.add_header('User-Agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0')
    req.add_header('Accept', 'application/json;q=0.9')
    req.add_header('Accept-Language', 'en-US,en;q=0.5')

    with urlopen(req) as response:
        return loads(response.read().decode('utf-8'))


def err_out():
    print("Exiting...")
    exit(1)


def validate_url(url):
    if not validators.url(url):
        print("Invalid URL: " + url)
        err_out()


if __name__ == "__main__":
    print("Fetching MIME types...")
    mime_types = get_mime_types('https://s-randomfiles.s3.amazonaws.com/mime/allMimeTypes.json')
    print("MIME types fetched.")

    print("Parsing MIME types...")
    parsed = parse_mime_types(mime_types).values()
    mime_types = sorted(parsed, key=lambda x: x['constructor'])
    print("MIME types parsed.")

    print("Writing Brigid.Types.MIME_Types...")
    create_module(mime_types)

    print("Done.")
    exit(0)
