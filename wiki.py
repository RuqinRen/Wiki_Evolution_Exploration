import hashlib
import os
from multiprocessing import Process, Queue
import requests
from lxml import etree
import re
import datetime

from lxml.etree import tostring


def hex_md5(s):
    m = hashlib.md5()
    m.update(s.encode('UTF-8'))
    return m.hexdigest()


def getFilterData():
    url = 'https://tools.wmflabs.org/enwp10/cgi-bin/list2.fcgi?run=yes&projecta=Aquarium_Fishes&namespace=&pagename=&quality=&importance=&score=&limit=1000&offset=1&sorta=Quality&sortb=Quality'
    html_info = requests.get(url).content
    html = etree.HTML(html_info)
    main_info = html.xpath('//table[contains(@class,"wikitable")]/tr')
    print(len(main_info))
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


def getAboutId(num, url):
    html_info = requests.get(url).content
    patternWgPageName = re.compile(r'"wgPageName":"([\s\S]*?)",', re.S)
    patternWgArticleId = re.compile(r'"wgArticleId":([\s\S]*?),', re.S)
    WgPageName = str(num) + '_' + patternWgPageName.findall(str(html_info))[0]
    WgArticleId = patternWgArticleId.findall(str(html_info))[0]
    WgPageName = WgPageName.replace("\\", '')
    os.rename('result/' + WgPageName, 'result/' + WgPageName + '_' + str(WgArticleId))
    if not os.path.exists('result/' + WgPageName):
        os.mkdir('result/' + WgPageName)
    html = etree.HTML(html_info)
    main_info = html.xpath('//ul[contains(@id,"pagehistory")]/li')
    for m in main_info:
        revid = m.get("data-mw-revid")
        try:
            updateDate = m.xpath('a//text()')[0]
            if int(updateDate.split(' ')[-1]) >= 2017:
                # print(updateDate)
                times = datetime.datetime.strptime(updateDate, "%H:%M, %d %B %Y")
                times = times.strftime("%Y_%m_%d_%H_%M_")
                oldIdUrl = 'https://en.wikipedia.org' + m.xpath('a//@href')[0]
                oldIdUrlHtmlContent = requests.get(oldIdUrl)
                oldIdUrlHtml = etree.HTML(oldIdUrlHtmlContent.content)
                oldIdUrlHtmlContentMainInfo = oldIdUrlHtml.xpath('//div[contains(@id,"bodyContent")]//text()')
                save2local('result/' + WgPageName + '/' + times + str(revid) + ".txt",
                           tostring(oldIdUrlHtmlContentMainInfo))
                print(WgPageName)
                oldIdUrlHtmlContent.close()
        except:
            print(num, url)


def save2local(filename, content):
    content = content.decode('utf-8')
    with open(filename, "w") as f:
        f.write(content)


# getAboutId('https://en.wikipedia.org/w/index.php?title=Pelvicachromis_pulcher&offset=&limit=100&action=history')

def job(q_result):
    while not q_result.empty():
        result = q_result.get()
        data_history_url = result[3] + '&limit=250'
        num = result[0]
        getAboutId(num, data_history_url)
        # print(num, result[1])


if __name__ == '__main__':
    result = getFilterData()
    q_result = Queue()
    for r in result:
        q_result.put(r)
    for i in range(40):
        p = Process(target=job, args=(q_result,))
        p.start()

    # getAboutId('27', 'https://en.wikipedia.org/w/index.php?title=Butterfly%20splitfin&action=history&limit=500')


# 获取一个日期是当月第几周
def get_week_of_month(year, month, day):
    begin = int(datetime.date(year, month, 1).strftime("%W"))
    end = int(datetime.date(year, month, day).strftime("%W"))
    return end - begin + 1


# 或取某个日期的周日
def get_current_week(year, month, day):
    sunday = datetime.date(year, month, day)
    one_day = datetime.timedelta(days=1)
    while sunday.weekday() != 6:
        sunday += one_day
    return sunday
