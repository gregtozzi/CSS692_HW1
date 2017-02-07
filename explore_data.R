require(igraph)
source('helper_functions.R')

# Load the data
Korea1 <- korea_graph('Korea1.net',
                      'Korea1_members.csv',
                      'Korea1_adopters.csv')

Korea2 <- korea_graph('Korea2.net',
                      'Korea2_members.csv',
                      'Korea2_adopters.csv')

Korea1DF <- as_data_frame(Korea1, what = 'vertices')
Korea2DF <- as_data_frame(Korea2, what = 'vertices')
