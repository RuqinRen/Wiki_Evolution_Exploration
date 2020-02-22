import json
from multiprocessing import Process, Queue

import requests
from lxml import etree


def getMainInfo(pageName, rvcontinue='|'):
    url = 'https://en.wikipedia.org/w/api.php'
    params = {
        'format': 'json',
        'action': 'query',
        'prop': 'revisions',
        'titles': pageName,
        'rvlimit': 50,
        'rvend': '2017-01-01T00:00:00Z',
        'rvprop': 'ids|timestamp|user|userid|content',  #
        'rvcontinue': rvcontinue,
    }
    revisions = []
    pageid = '--------------'
    try:
        json_info = requests.get(url, params=params).content
        j = json.loads(json_info)
        pageid = list(j['query']['pages'].keys())[0]
        revisions = j['query']['pages'][pageid]['revisions']
        if j.get('continue'):
            pageid, temp = getMainInfo(pageName, j.get('continue')['rvcontinue'])
            revisions.extend(temp)
    except:
        pass
    return pageid, revisions


def save2local(filename, content):
    # content = content.decode('utf-8')
    with open(filename, "w", encoding='utf-8') as f:
        json.dump(content, f)


def read4loacl(filename):
    temp = None
    with open(filename, "r", encoding='utf-8') as f:
        temp = json.load(f)
    return temp


def getFilterData():
    url = 'https://tools.wmflabs.org/enwp10/cgi-bin/list2.fcgi?run=yes&projecta=Aquarium_Fishes&namespace=&pagename=&quality=&importance=&score=&limit=1000&offset=1&sorta=Quality&sortb=Quality'
    html_info = requests.get(url).content
    html = etree.HTML(html_info)
    main_info = html.xpath('//table[contains(@class,"wikitable")]/tr')
    filter = ['GA', 'B', 'C', 'Start']
    result = []
    for m in main_info[1:]:
        Quality = m.xpath('td[5]//text()')
        if ''.join(Quality) in filter:
            resultnum = m.xpath('td[1]/text()')[0]
            ArticleName = m.xpath('td[2]/a[1]/text()')[0]
            ArticleURL = m.xpath('td[2]/a[1]/@href')[0]
            ArticleHistoryURL = m.xpath('td[2]/a[3]/@href')[0]
            Score = m.xpath('td[9]//text()')[0]
            result.append([resultnum, ArticleName, ArticleURL, ArticleHistoryURL, Quality[0], Score])
    return result


def job(q_result):
    while not q_result.empty():
        result = q_result.get()
        num = result[0]
        pageName = result[1]
        temp_result = []
        pageid = '--------------'
        try:
            pageid, temp_result = getMainInfo(pageName)
            # print(num, len(temp_result), result[1])
            parentid = temp_result[-1]['parentid']
            _, previous = getPrevious(parentid, pageName)
            temp_result.extend(previous)
        except:
            temp_result = []
            pageid = '--------------'
            print(num, pageName)
        save2local(
            'result/' + str(num) + '_' + result[1] + '_' + str(pageid) + '_' + str(len(temp_result)) + '.txt',
            temp_result)


def getOne(num, pageName):
    pageid, temp_result = getMainInfo(pageName)
    print(temp_result)
    parentid = temp_result[-1]['parentid']
    _, previous = getPrevious(parentid, pageName)
    temp_result.extend(previous)
    save2local('result/' + str(num) + '_' + pageName + '_' + str(pageid) + '_' + str(len(temp_result)) + '.txt',
               temp_result)


# 得到第一条数据的前一条
def getPrevious(parentid, articleName):
    url = 'https://en.wikipedia.org/w/api.php'
    params = {
        'format': 'json',
        'action': 'query',
        'prop': 'revisions',
        'titles': articleName,
        'rvstartid': parentid,
        'rvendid': parentid,
        'rvprop': 'ids|timestamp|user|userid|content',  #
    }

    revisions = []
    json_info = requests.get(url, params=params).content
    j = json.loads(json_info)
    pageid = list(j['query']['pages'].keys())[0]
    revisions = j['query']['pages'][pageid]['revisions']
    return pageid, revisions


if __name__ == '__main__':
    pass
    # result = getFilterData()
    # print(len(result))
    # q_result = Queue()
    # for r in result:
    #     q_result.put(r)
    # for i in range(20):
    #     p = Process(target=job, args=(q_result,))
    #     p.start()
    # pageid, temp_result = getMainInfo('Synodontis_decorus')
    # getOne('360', 'Pierre Carbonnier')
