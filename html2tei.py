from bs4 import BeautifulSoup
import re
from random import choice

class Book():
    """ A container for books, that contains objects like chapters. """
    def __init__(self, text):
        soup = BeautifulSoup(text, features='lxml')
        self.chapters = self.getChapters(soup)
        self.nChapters = len(self.chapters)

    def __repr__(self):
        out = "Book with {} chapters\n".format(self.nChapters)
        out += "Chapters: {}".format(self.chapters)
        return out

    def getChapters(self, soup):
        """
        Case 1: Chapters are <divs> that contain <a name="#link2HCH0001"> tags, where
        CH0001 refers to chapter 1.
        """
        divsWithAs = [n.findParent('div') for n in soup.findAll('a') if 'name' in n.attrs]
        chaps = [Chapter(div) for div in divsWithAs]
        return chaps

class Chapter():
    """ A chapter, which contains objects like paragraphs. """
    def __init__(self, divSoup):
        self.divSoup = divSoup
        self.nParas = len(self.paras)

    def __repr__(self):
        out = "  Chapter labeled {} with {} paragraphs.\n".format(self.label, self.nParas)
        out += "    Heading: {}".format(self.heading)
        # out += "       Paras: {}".format(self.paras)
        return out

    @property
    def label(self):
        asWithNames = [n for n in self.divSoup.findAll('a')
                       if 'name' in n.attrs]
        labelCandidates = asWithNames
        if len(labelCandidates) > 1:
            print("More than one possible label here: ", labelCandidates)
        elif labelCandidates == []:
            print('Too few label candidates.')
        else:
            return labelCandidates[0].attrs['name']

    @property
    def heading(self):
        """ Choose a heading from among the headings gathered in self.headings(). """
        headings = self.headings
        nonNone = [headings[item] for item in self.headings if headings[item] is not None]
        if nonNone != []:
            return nonNone[0].getText().strip()

    @property
    def headings(self):
        """ Find all headings, <h1> through <h6>. """
        return {n:self.divSoup.findNext('h'+str(n)) for n in [1, 2, 3, 4, 5, 6]}

    @property
    def paras(self):
        return [Para(node) for node in self.divSoup.findAll('p')]

class Para():
    """ A paragraph, which mostly contains the text, but could also contain other markup. """
    def __init__(self, para):
        self.markup = para
        self.text = self.markup.getText()
        # No line breaks in this one, for regex, etc.

    def __len__(self):
        return len(self.text.split())

    def __repr__(self):
        out = "{}w ".format(len(self))
        out += "Text: {}".format(self.text)
        out += "Cleaned: {}\n".format(self.cleanText)
        out += "Quot: {}".format(self.quotations)
        return out

    @property
    def cleanText(self):
        clean = self.text.replace('\n', ' ')
        clean = re.sub(r'\s+', ' ', clean)
        return clean

    @property
    def quotations(self):
        """
        Extract quotations from paragraphs
        TODO: Handle straight quotes
        TODO: Refactor this
        """
        quotData = {  # Case: "But you forget, mamma," said Elizabeth,
                      # "that we shall meet him at the assemblies!"
                    'interrupted': {'pattern': r'(“.*?[,!?]”)(.*?)(“.*?”)',
                                    'quotedGroups': [1, 3],
                                    'betweenGroup': 2
                                    },
                    # Sometimes the final quotation mark at the end of a paragraph
                    # Is omitted. 
                    'paragraph':   {'pattern': '(“.*?$)',
                                    'quotedGroups':  [0],
                                    'betweenGroup': ''
                                    },
                    'normal':       {'pattern': r'(“.*?”)',
                                     'quotedGroups': [0],
                                     'betweenGroup': ''
                                     }
                    }

        for quotType, data in quotData.items():
            matches = re.finditer(data['pattern'], self.cleanText, re.UNICODE)
            if matches is not None and matches is not []:
                return [Quotation(quotType,
                                  quoted=[match.group(n) for n in data['quotedGroups']],
                                  between=match.group(data['betweenGroup']))
                        for match in matches]


class Quotation():
    def __init__(self, quotType, quoted, between=""):
        # types can be "interrupted," "paragraph," or "other"
        self.quotType = quotType
        # said Elizabeth
        self.between = between
        self.quoted = quoted
    def __repr__(self):
        return "Type: {}, between: {}, quoted: {}".format(
            self.quotType, self.between, self.quoted)

def testRandomParagraph(fn): 
    with open(fn) as f:
        rawText = f.read()
        bookObj = Book(rawText)
        randomChap = bookObj.chapters[choice(range(0,bookObj.nChapters))]
        randomPara = randomChap.paras[choice(range(0,randomChap.nParas))]
    print(randomPara)
    return 

print(testRandomParagraph('1342-h.htm'))
