



complicate = c("complicated", "complication" , "complicatedly")
stem_doc = stemDocument(complicate)
comp_dict = "complicate"
complete_text = stemCompletion(stem_doc , comp_dict)
complete_text




text_data = "In a complicated haste , Tom rushed to fix a new complication, too complicatedly"
rm_punc = removePunctuation(text_data)
n_char_vec = unlist(strsplit(rm_punc, split = ' '))
n_char_vec
stem_doc  = stemDocument(n_char_vec)
stem_doc
complete_doc = stemCompletion( stem_doc , comp_dict )
complete_doc








