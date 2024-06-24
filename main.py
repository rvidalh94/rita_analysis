import pandas as pd
import os
from openai import OpenAI
import re
import heapq
from sklearn.feature_extraction.text import TfidfVectorizer

# Initialize OpenAI API
openai_api_key = os.environ.get("OPENAI_API_KEY")
client = OpenAI(api_key=openai_api_key)

def filter_relevant_thesaurus_words(text, thesaurus_words, max_words=200):
    try:
        # Use TF-IDF to select the most relevant words from the thesaurus
        vectorizer = TfidfVectorizer(vocabulary=thesaurus_words)
        tfidf_matrix = vectorizer.fit_transform([text])
        feature_names = vectorizer.get_feature_names_out()
        scores = tfidf_matrix.toarray()[0]
        
        # Get the top `max_words` relevant thesaurus words
        top_indices = heapq.nlargest(max_words, range(len(scores)), scores.take)
        top_words = [feature_names[i] for i in top_indices]
        
        return top_words
    except Exception as e:
        print(f"Error: {e}")
        return None

def extract_keywords(text, thesaurus_words, N=5):
    relevant_thesaurus_words = filter_relevant_thesaurus_words(text, thesaurus_words)
    if relevant_thesaurus_words is None:
        return None
    prompt = f"Assign {N} keywords to the following abstract according to the thesaurus word database. \n\nAbstract: {text}\n\nThesaurus database: {', '.join(relevant_thesaurus_words)}\n\Return the keywords separated by a comma e.g. medical systems, arduino, metheorology"
    
    response = client.chat.completions.create(
        model="gpt-3.5-turbo",
        messages=[
            {"role": "system", "content": "You are a helpful assistant that extracts keywords from texts."},
            {"role": "user", "content": prompt}
        ],
        max_tokens=60,
        n=1,
        stop=None,
        temperature=0.5
    )
    keywords = response.choices[0].message.content
    return [keyword.strip() for keyword in keywords.split(',') if keyword]

# Load the thesaurus words from the ieee-taxonomy file
thesaurus_df = pd.read_csv('ieee-taxonomy.csv')
thesaurus_words = set(thesaurus_df.iloc[:, 0].str.lower())

# Load the main CSV file
main_df = pd.read_csv('BD_RITA_CONSOLIDATED.csv')

# Process each row in the main dataframe
for index, row in main_df.iterrows():
    abstract = row['abstract']
    if pd.isna(row['keywords']):  # Check if 'keywords' column is empty
        print("Analyzing abstract: ", abstract)
        extracted_keywords = extract_keywords(abstract, thesaurus_words=thesaurus_words)
        if extracted_keywords is None:
            continue
        print(f"{extracted_keywords=}")
        main_df.at[index, 'keywords'] = ', '.join(extracted_keywords)

# Save the updated dataframe back to CSV
main_df.to_csv('BD_RITA_CONSOLIDATED_updated.csv', index=False)