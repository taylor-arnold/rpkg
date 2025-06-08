# -*- coding: utf-8 -*-
"""Use the spacy library to extract linguistic features.
"""

from warnings import catch_warnings, simplefilter

import spacy


class spacyCleanNLP:
    """A class to call spacy and output normalized tables"""

    def __init__(self, model_name='en', max_length=1000000, disable=None):
        if disable is None:
            disable = []

        if isinstance(disable, str):
            disable = [disable]

        with catch_warnings():
            simplefilter("ignore")
            try:
                self.nlp = spacy.load(
                    name=model_name,
                    disable=disable
                )
                if 'parser' not in self.nlp.pipe_names:
                    if 'sentencizer' not in self.nlp.pipe_names:
                        self.nlp.add_pipe(self.nlp.create_pipe('sentencizer'))

            except OSError as e:
                self.nlp = None

    def parseDocument(self, text, doc_id):
        with catch_warnings():
            simplefilter("ignore")
            doc = self.nlp(text)

        sent_index = {}
        sid = 1
        for sent in doc.sents:
            sent_index[sent.start] = sid
            sid += 1

        token = get_token(doc, doc_id)
        ent = get_entity(doc, doc_id, sent_index)

        return {"token": token, "entity": ent}


def get_token(doc, doc_id):
    token = {
        "doc_id": [],
        "sid": [],
        "tid": [],
        "token": [],
        "token_with_ws": [],
        "lemma": [],
        "upos": [],
        "xpos": [],
        "tid_source": [],
        "relation": []
    }

    sid = 1
    for x in doc.sents:
        # spacy counts tokens over the whole doc, but we
        # want it within a sentence as with corenlp. So
        # save the first at sentence start and substract
        # it off
        start_token_i = x[0].i

        # Now, parse the actual tokens, starting at 1
        tid = 1
        for word in x:
            if word.dep_ == "ROOT":
                dep_id = 0
            else:
                dep_id = word.head.i - start_token_i + 1

            token['doc_id'].append(doc_id)
            token['sid'].append(sid)
            token['tid'].append(tid)
            token['token'].append(word.text)
            token['token_with_ws'].append(word.text_with_ws)
            token['lemma'].append(word.lemma_)
            token['upos'].append(word.pos_)
            token['xpos'].append(word.tag_)
            token['tid_source'].append(dep_id)
            token['relation'].append(word.dep_)

            tid += 1
        sid += 1

    return token


def get_entity(doc, doc_id, sent_index):
    evals = {
        "doc_id": [],
        "sid": [],
        "tid": [],
        "tid_end": [],
        "entity_type": [],
        "entity": []
    }

    for ent in doc.ents:

        sid = sent_index.get(ent.sent.start, -1)
        tid_start = ent.start - ent.sent.start + 1
        tid_end = ent.end - ent.sent.start
        entity = ent.text.replace('"', '')

        evals['doc_id'].append(doc_id)
        evals['sid'].append(sid)
        evals['tid'].append(tid_start)
        evals['tid_end'].append(tid_end)
        evals['entity_type'].append(ent.root.ent_type_)
        evals['entity'].append(entity)

    return evals
