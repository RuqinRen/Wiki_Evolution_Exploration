import datetime
import json
import re
import time
from multiprocessing import Queue
from ores import ores
from textstat.textstat import textstat
import pageviewapi
import arrow
import requests
import articlequality
from revscoring import Model


def isLeapYear(years):
    assert isinstance(years, int), "请输入整数年，如 2018"
    if ((years % 4 == 0 and years % 100 != 0) or (years % 400 == 0)):  # 判断是否是闰年
        # print(years, "是闰年")
        days_sum = 366
        return days_sum
    else:
        # print(years, '不是闰年')
        days_sum = 365
        return days_sum


def get_current_week(d):
    year, month, day = d.split('-')
    sunday = datetime.date(int(year), int(month), int(day))
    one_day = datetime.timedelta(days=1)
    while sunday.weekday() != 6:
        sunday += one_day
    return datetime.datetime.strptime(sunday.strftime('%Y-%m-%d 23:59:59'), '%Y-%m-%d %H:%M:%S')


def get_current_week2(d):
    year, month, day = d[:4], d[4:6], d[6:]
    sunday = datetime.date(int(year), int(month), int(day))
    one_day = datetime.timedelta(days=1)
    while sunday.weekday() != 6:
        sunday += one_day
    return sunday.strftime('%Y%m%d')


def get_current_monday(d):
    monday = d.date()
    one_day = datetime.timedelta(days=1)
    while monday.weekday() != 0:
        monday -= one_day
    return monday.strftime('%Y%m%d')


def getAllDayPerYear(years):
    start_date = '%s-1-1' % years
    a = 0
    all_date_list = []
    days_sum = isLeapYear(int(years))
    while a < days_sum:
        b = arrow.get(start_date).shift(days=a).format("YYYY-MM-DD")
        a += 1
        all_date_list.append(b)
    # print(all_date_list)
    return all_date_list


def getAllDayOfSunday():
    all_date_list = []
    for i in [2017, 2018, 2019, 2020]:
        all_date_list.extend(getAllDayPerYear(i))
    sundayList = set()
    for i in all_date_list:
        sundayList.add(get_current_week(i))
    #
    # sundayDict = {}
    # for i in sundayList:
    #     sundayDict[i.strftime('%Y-%W')] = i
    return sorted(list(sundayList))


def getNumOfreferences(article):
    refPatternWg = re.compile(r'<ref([\s\S]*?)</ref>', re.S)
    numOfreferences = refPatternWg.findall(article)
    return len(numOfreferences)


def getNumOfCategories(article):
    # [[Category:
    categoryPatternWg = re.compile(r'\[\[Category:([\s\S]*?)\]\]', re.S)
    numOfcategorys = categoryPatternWg.findall(article)
    return len(numOfcategorys)


def getNumOfLv2Heading(article):
    lv2HeadingPatternWg = re.compile(r'==([\s\S]*?)==', re.S)
    numOflv2Heading = lv2HeadingPatternWg.findall(article)
    return len(numOflv2Heading)


def hasInfoBox(article):
    infoBoxPatternWg = re.compile(r'{{.*?box[\s\S]*?}}')
    infoBox = infoBoxPatternWg.findall(article)
    if len(infoBox):
        return True
    return False


def getNumOfCiteTemp(article):
    URL = "https://www.mediawiki.org/w/api.php"
    params = {
        'action': 'parse',
        'text': article,
        'format': 'json',
        'title': 'main_page',
        'prop': 'templates|links|images|rev_len',
    }
    flag = True
    while flag:
        try:
            html = requests.post(url=URL, data=params)
            j = json.loads(html.text)
            flag = False
        except:
            print('重试getNumOfCiteTemp')
            time.sleep(4)
    html.close()
    time.sleep(2)
    return len(j['parse']['templates']), len(j['parse']['links']), len(j['parse']['images'])


def getContent(article: str):
    flag = True
    while flag:
        Pattern1 = re.compile(r'{{[^{}]*?}}')
        if Pattern1.findall(article):
            article = Pattern1.sub('', article)
        else:
            flag = False
    Pattern2 = re.compile(r'(<ref[^<>]*?/>)')
    article = Pattern2.sub('', article)
    Pattern3 = re.compile(r'(<ref[\s\S]*?</ref>)')
    article = Pattern3.sub('', article)
    Pattern4 = re.compile(r'(\[\[Category:[\s\S]*?\]\])')
    article = Pattern4.sub('', article)
    Pattern5 = re.compile(r'(File:[\s\S]*?\n)')
    article = Pattern5.sub('', article)
    Pattern6 = re.compile(r'(==[ ]*References[ ]*?==[\s\S]*)')
    article = Pattern6.sub('', article)
    Pattern7 = re.compile(r'(<[^<>]*?>)')
    article = Pattern7.sub('', article)
    article = article.replace('[[', '').replace(']]', '').replace("'''", '')
    article = article.replace('===', '').replace('==', '').replace('\n', '')
    return article


def getContentLength(article):
    return len(article)
    # for i in article:
    #     print(i)


def getHtml(article):
    URL = "https://www.mediawiki.org/w/api.php"
    params = {
        'action': 'parse',
        'text': article,
        'format': 'json',
        'title': 'main_page',
        'prop': 'text',
    }
    html = requests.post(url=URL, data=params).content
    j = json.loads(html)
    print(j['parse']['text']['*'])


def getFlesch_reading_ease(article):
    return textstat.flesch_reading_ease(article)


def getColeman_liau_index(article):
    return textstat.coleman_liau_index(article)


def getDifficult_words(article):
    return textstat.difficult_words(article)


def getPageview(articleName, sundayList):
    resultTemp = []
    flag = True
    while flag:
        try:
            d = pageviewapi.per_article('en.wikipedia', articleName, '20170101', '20200223',
                                        access='all-access', agent='all-agents', granularity='daily')
            for i in list(d['items']):
                resultTemp.append(
                    {'timestamp': i['timestamp'][:-2], 'views': i['views']})
                # datetime.datetime.strptime(i['timestamp'], '%Y%m%d00')
            if d:
                flag = False
        except:
            print(articleName, '重试getPageview')
            time.sleep(4)
    result = {}
    for s in sundayList:
        result[s.strftime('%Y%m%d')] = 0
    for rt in resultTemp:
        timestamp = get_current_week2(rt['timestamp'])
        result[timestamp] += int(rt['views'])
    return result


def getQuality(ls):
    l_queue = []
    for l in ls:
        l_queue.append(str(l['revid']))
    leftFlag = 0
    step = 50
    rvidTemp = []
    while leftFlag + step < len(l_queue):
        rvidTemp.append(l_queue[leftFlag:leftFlag + step])
        leftFlag += step
    rvidTemp.append(l_queue[leftFlag:])
    quality = {}
    for rt in rvidTemp:
        url = 'https://ores.wikimedia.org/v3/scores/enwiki/?models=articlequality'
        params = {
            'models': 'articlequality',
            'revids': '|'.join(rt)
        }
        flag = True
        while flag:
            try:
                html = requests.get(url, params=params)
                htmlInfo = html.json()
                scores = htmlInfo['enwiki']['scores']
                leastPrediction = None
                for s in scores:
                    if scores[s]['articlequality'].get('score'):
                        quality[s] = scores[s]['articlequality']['score']['prediction']
                        leastPrediction = quality[s]
                    else:
                        quality[s] = leastPrediction
                html.close()
                flag = False
            except:
                print('重试getQuality')
                time.sleep(4)
    return quality
