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

    def __len__(self):
        return len(self.text.split())

    def __repr__(self):
        out = "{}w ".format(len(self))
        out += "Text: {}\n".format(self.text)
        out += "Cleaned: {}\n".format(self.cleanText)
        out += "Quot: {}\n".format(self.quotations)
        out += "Percent quoted: {}\n".format(self.percentQuoted)
        return out

    @property
    def percentQuoted(self):
        """ How much of this paragraph is dialogue? (quotation) """
        if self.quotations:
            return len(self.quotations) / len(self)
        else:
            return 0

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
        """
        quotData = {
            # Case: "But you forget, mamma," said Elizabeth,
            # "that we shall meet him at the assemblies!"
                    'interrupted': {'pattern': r'(“.*?[,!?]”)(.*?)(“.*?”)',
                                    'quotedGroups': [1, 3],
                                    'betweenGroup': 2
                    },
            # Sometimes the final quotation mark at the end of a paragraph
            # Is omitted.
                    'paragraph':   {'pattern': '(^“.*?[”$])',
                                    'quotedGroups':  [0],
                                    'betweenGroup': ''
                    },
                    'normal':       {'pattern': r'“.*?”',
                                    'quotedGroups': [0],
                                    'betweenGroup': ''
                    }
        }

        for quotType, data in quotData.items():
            matches = list(re.finditer(data['pattern'], self.cleanText, re.UNICODE))
            if matches:
                print('matches!', matches)
                for match in matches:
                    quoted = [match.group(n) for n in data['quotedGroups']]
                    if data['betweenGroup']:
                        between = match.group(data['betweenGroup'])
                    else:
                        between = ""
                    return Quotation(quotType, quoted, between)
    
class Quotation():
    def __init__(self, quotType, quoted, between=""):
        # types can be "interrupted," "paragraph," or "other"
        self.quotType = quotType
        # said Elizabeth
        self.between = between
        self.quoted = quoted
        self.speaker, self.person, self.verb = self.getSpeaker(between)

    def __repr__(self):
        out = f"Type: {self.quotType}, between: {self.between}, quoted: {self.quoted}\n"
        out += f"Speaker: {self.speaker}, Person: {self.person}\n"
        return out

    def __len__(self):
        """ Get length in number of words. """
        return len(' '.join(self.quoted).split())

    def _removeTrailingPunctuation(self, text):
        """
        Takes a string, and returns that string without any
        trailing punctuation.
        """

        # Delete ending punctuation
        text = text.strip()

        if text[-1:] in ",.;:-":
            text = text[:-1]
        return text

    def getSpeaker(self, between):
        """
        Takes what is in-between an interrupted quotation and returns the speaker,
        the verb ("cried," "said," etc.), and anything else.
        Returns:
         - speaker: who speaks (could be "he," "she," etc.)
         - person: name of the person speaking (Mr. Darcy)
        """

        verbs = ["said", "cried", "returned", "replied", "observed", "continued", "thought", "repeated", "added", "answered"]
        pronoun = ["he", "she"]
        title = ["Mr.", "Mrs.", "Miss", "Sir"]
        person = set()
        speaker = []

        line = self._removeTrailingPunctuation(between)

        # Set speaker
        speaker_line = ''

        line_split = line.split()

        for i in range(len(line_split)):
            if line_split[i] in verbs:
                if i+1 < len(line_split): 
                    # said he
                    if line_split[i+1] in pronoun:
                        speaker_line = line_split[i+1]
                    # said Mr. Collins
                    elif line_split[i+1] in title:
                        speaker_line = line_split[i+1] + " " + line_split[i+2]
                        person.add(speaker_line)
                    # said Elizabeth, said Colonel Fitzwilliam
                    elif line_split[i+1][:1].isupper():
                        if i+2 < len(line_split):
                            if line_split[i+2][:1].isupper():
                                speaker_line = line_split[i+1] + " " + line_split[i+2]
                                person.add(speaker_line)
                        else:
                            speaker_line = line_split[i+1]
                            person.add(speaker_line)
        if not speaker_line:
            speaker_line = line
        speaker.append(speaker_line)

        # FIXME
        verb = ""

        return speaker, person, verb


def testRandomParagraph(fn):
    with open(fn, encoding = "utf-8") as f:
        rawText = f.read()
        bookObj = Book(rawText)
        randomChap = bookObj.chapters[choice(range(0,bookObj.nChapters))]
        # randomPara = randomChap.paras[choice(range(0,randomChap.nParas))]
        paras = randomChap.paras
        allQuots = []
        for para in paras:
            if para.quotations is not None:
                allQuots.append(para.quotations)
            else:
                continue
        interruptedQuotations = [quot for quot in allQuots if quot.quotType == "interrupted"]

    print(interruptedQuotations)
    return 

print(testRandomParagraph('1342-h.htm'))
#print(findAllBetween('1342-h.htm'))
    
# print(speaker)
# print(person)
