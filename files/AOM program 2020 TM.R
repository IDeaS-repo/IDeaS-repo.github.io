#########################################
### Package installation
#########################################
# The "tm" package enables the text mining infrastructure that we will use for LDA.
require("tm")
require("topicmodels")
require("stm")


#########################################
### Get the data, turn into a corpus, and clean it up
#########################################
# Load data from a URL
data = read.csv(url("https://github.com/IDeaS-repo/IDeaS-repo.github.io/blob/master/files/OMT_2015-2020.csv?raw=true"))

# Create a corpus. 
corpus = VCorpus((VectorSource(data[, "title_ab"])))

# Basic cleaning (step-wise)
# We write everything to a new corpus called "corpusclean" so that we do not lose the original data.
# 1) Remove numbers
corpusclean = tm_map(corpus, removeNumbers)
# 2) Remove punctuation
corpusclean = tm_map(corpusclean, removePunctuation)
# 3) Transform all upper-case letters to lower-case.
corpusclean = tm_map(corpusclean,  content_transformer(tolower))
# 4) Remove stopwords which do not convey any meaning.
corpusclean = tm_map(corpusclean, removeWords, stopwords("english")) 
# this stopword file is at C:\Users\[username]\Documents\R\win-library\[rversion]\tm\stopwords 

# i	me	my	myself	we	our	ours	ourselves	you	your	yours	yourself	yourselves	he	him	his	himself	
# she	her	hers	herself	it	its	itself	they	them	their	theirs	themselves	what	which	who	whom	this
# that	these	those	am	is	are	was	were	be	been	being	have	has	had	having	do	does	did	doing	would	should
# could	ought	i'm	you're	he's	she's	it's	we're	they're	i've	you've	we've	they've	i'd	you'd	he'd	she'd	we'd
# they'd	i'll	you'll	he'll	she'll	we'll	they'll	isn't	aren't	wasn't	weren't	hasn't	haven't	hadn't	doesn't	
# don't	didn't	won't	wouldn't	shan't	shouldn't	can't	cannot	couldn't	mustn't	let's	that's	who's	what's	here's
# there's	when's	where's	why's	how's	a	an	the	and	but	if	or	because	as	until	while	of	at	by	for	with	about	
# against	between	into	through	during	before	after	above	below	to	from	up	down	in	out	on	off	over	under	again
# further	then	once	here	there	when	where	why	how	all	any	both	each	few	more	most	other	some	such	no	nor	
# not	only	own	same	so	than	too	very

# 5) And strip whitespace. 
corpusclean = tm_map(corpusclean , stripWhitespace)

# We then convert the corpus to a "Document-term-matrix" (dtm)
dtm =DocumentTermMatrix(corpusclean)  

#########################################
### LDA: Running the model
#########################################
# We first fix the random seed for future replication.
SEED = 123456789

stmdata <- readCorpus(dtm, type = c("slam"))
stmdata$meta <- data

set.seed(123456789)
# Find number of topics:
# 
kResult <- searchK(stmdata$documents, stmdata$vocab, K=c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75), prevalence =~ s(year),
                   data=stmdata$meta, init.type = "Spectral", heldout.seed = 123456789,
                   max.em.its = 150, verbose = TRUE, control = list())
plot(kResult)
kResult
# Let's work with 45. 

topicnr = 45

stmmodel <- stm(stmdata$documents, stmdata$vocab, topicnr,prevalence =~ s(year),
                  data = stmdata$meta,
                  init.type = "Spectral", seed = 123456789,
                  max.em.its = 75, verbose = TRUE, reportevery = 5,
                  control = list())

labels <- labelTopics(stmmodel, n = 5)
labels


prep <- estimateEffect(1:45 ~ s(year),
                       stmmodel, meta=stmdata$meta, 
                       uncertainty="Global")

summary(prep)
plot(prep, "year", method = "continuous", topics = 13,
     model = stmmodel, printlegend = FALSE, xlab = "Year",ci.level = 0, ylim = c(0.00, 0.05), main="Topic 13: failure, umbrella, constructs, resilience, hubris")
plot(prep, "year", method = "continuous", topics = 14,
     model = stmmodel, printlegend = FALSE, xlab = "Year",ci.level = 0, ylim = c(0.00, 0.05), main="Topic 14: occupational, community, occupations, communities, occupation")
plot(prep, "year", method = "continuous", topics = 16,
     model = stmmodel, printlegend = FALSE, xlab = "Year",ci.level = 0, ylim = c(0.00, 0.05), main="Topic 16: stigma, stigmatized, stigmatization, scandal, transgression")
plot(prep, "year", method = "continuous", topics = 21,
     model = stmmodel, printlegend = FALSE, xlab = "Year",ci.level = 0, ylim = c(0.00, 0.05), main="Topic 21: collective, process, sensemaking, narratives, framing")
plot(prep, "year", method = "continuous", topics = 26,
     model = stmmodel, printlegend = FALSE, xlab = "Year",ci.level = 0, ylim = c(0.00, 0.05), main="Topic 26: impression, frameworks, symbolic, stakeholder, managerial")
plot(prep, "year", method = "continuous", topics = 32,
     model = stmmodel, printlegend = FALSE, xlab = "Year",ci.level = 0, ylim = c(0.00, 0.05), main="Topic 32: identity, organizational, identities, organizations, work")
plot(prep, "year", method = "continuous", topics = 36,
     model = stmmodel, printlegend = FALSE, xlab = "Year",ci.level = 0, ylim = c(0.00, 0.05), main="Topic 36: leadership, study, research, responsible, leader")
# These were all identified via manual inspection. 
