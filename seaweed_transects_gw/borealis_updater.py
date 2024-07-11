'''
Read file list from a dataverse study and download new versions if available

'''
import os
import pathlib
import pickle # simple persistent data
# Requests library because the built-in tools are cumbersome
import requests #https://requests.readthedocs.io/en/latest/

# use an environment variable to store your API key if you want, in this case BKEY
KEY = os.environ.get('BKEY', '[your key]')
BASE = 'https://borealisdata.ca'
# saves the file data in the same directory as this file

def checkdata(pid:str='doi:10.5683/SP3/IKGB6E', key:str=KEY, timeout:int=100) ->dict:
    '''
    Returns a dict which indicates which files are different from the last check

    '''
    perdata_f = pathlib.Path(pathlib.Path(__file__).parent, f'{pid[-6:]}.pickle')
    #Get the current version first
    #https://guides.dataverse.org/en/5.6/api/native-api.html#list-versions-of-a-dataset
    vers = requests.get(f'{BASE}/api/datasets/:persistentId/versions',
                        params={'persistentId': pid},
                        headers={'X-Dataverse-key': key},
                        timeout=timeout)
    # In theory, the latest version is the first in the list, but given this is
    # dataverse we're dealing with it's safer not to assume
    versions = [(ind, ver['versionNumber']) for ind, ver in enumerate(vers.json()['data'])]
    current_index = [i[0] for i in versions if i[1]==max((x[1] for x in versions))][0]

    #https://guides.dataverse.org/en/5.6/api/native-api.html#list-files-in-a-dataset
    cur_files = requests.get((f'{BASE}/api/datasets/:persistentId/versions/'
                              f'{versions[current_index][1]}/files'),
                             params={'persistentId' : pid},
                             headers={'X-Dataverse-key' : key},
                             timeout=timeout)
    # If you need to check for changes, you need to check against something that persists
    # You could make a database, but in this case we will just check against a Python
    # pickle which stores the JSON from the http request
    if not perdata_f.exists(): # the first time you run this it won't exist
        with open(perdata_f, mode='wb') as perd:
            pickle.dump({'data':[]}, perd)
    # yes this is redundant, but only once so an elegant solution to reading
    # and rewriting is kind of pointless
    with open(perdata_f, mode='rb') as perd:
        perdata = pickle.load(perd)
    out = [x for x in cur_files.json()['data'] if x not in perdata['data']]
    #Now save the most recent data if there are any changes
    if out:
        with open(perdata_f, mode='wb') as perd:
            pickle.dump(cur_files.json(), perd)
    return out

def download_file(key:str=BASE, timeout:int=100, **kwargs)->None:
    '''
    Download a file from Dataverse; uses the output from checkdata
    optional parameter:
        outdir : str
            output directory
    '''
    #https://guides.dataverse.org/en/5.6/api/getting-started.html#downloading-files
    #gets a file name based on first the original then .tab file name
    if kwargs.get('outdir'):
        os.makedirs(kwargs['outdir'], exist_ok=True)
    name = kwargs['dataFile'].get('originalFileName', kwargs['label'])

    #The unique file ID
    fid = kwargs['dataFile']['id']

    file = requests.get(f'{BASE}/api/access/datafile/{fid}',
                        headers={'X-Dataverse-key': key},
                        params={'format':'original'},
                        timeout=timeout)
    file.raise_for_status()

    with open(pathlib.Path(kwargs.get('outdir'), name), 'wb') as outf:
        outf.write(file.content)

if __name__ == '__main__':
    #This will automatically download updated files for
    #https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/IKGB6E
    new_files = checkdata()
    if not new_files:
        print('No updated data')
    for new_file in new_files:
        print(f'Downloading {new_file["dataFile"].get("originalFileName", new_file["label"])}')
        download_file(**new_file, outdir='data')
