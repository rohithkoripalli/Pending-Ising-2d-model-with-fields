# The input .json file was converted to .csv with the code in another file
# The code was written only by taking some attributes into account
# This was done by  using  graph lab, Firstly the given data is loaded and the  unwanted movies were seperated based on their ratings and their titles, genre was extracted
# The data was trained and validated.
# Then by using the default commands the similar movies were extracted based on the target assigned
 


from os import path
import graphlab as gl
from datetime import datetime
from graphlab.recommender import item_similarity_recommender

# Path to the dataset directory
data_dir = './dataset/ml-20m'

# Table of movies we are recommending: movieId, title, genres
items = gl.SFrame.read_csv(path.join(data_dir, ''))

# Table of interactions between users and items: userId, movieId, rating, timestamp
actions = gl.SFrame.read_csv(path.join(data_dir, 'ratings.csv'))

###  The loaded data is  Prepare Data ###

# Prepare the data by removing items that are rare
rare_items = actions.groupby('movieId', gl.aggregate.COUNT).sort('Count')
rare_items = rare_items[rare_items['Count'] <= 5]
items = items.filter_by(rare_items['movieId'], 'movieId', exclude=True)

actions = actions[actions['rating'] >=4 ]
actions = actions.filter_by(rare_items['movieId'], 'movieId', exclude=True)

# Extract year, title, and genre
items['year'] = items['title'].apply(lambda x: x[-5:-1])
items['title'] = items['title'].apply(lambda x: x[:-7])
items['genres'] = items['genres'].apply(lambda x: x.split('|'))
actions['timestamp'] = actions['timestamp'].astype(datetime)

# Get the metadata ready
urls = gl.SFrame.read_csv(path.join(data_dir, 'movie_urls.csv'))
items = items.join(urls, on='movieId')
users = gl.SFrame.read_csv(path.join(data_dir, 'user_names.csv'))

training_data, validation_data = gl.recommender.util.random_split_by_user(actions, 'userId', 'movieId')

### Train similar Model ###

model = item_similarity_recommender.create(training_data,target='rating',only_top_k=10)

# Interactively evaluate and explore recommendations
view = model.views.overview(observation_data=training_data,
                            validation_set=validation_data,
                            user_data=users,
                            user_name_column='name',
                            item_data=items,
                            item_name_column='title',
                            item_url_column='url')
view.show()

