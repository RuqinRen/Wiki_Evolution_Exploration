import os
import csv

from utils import *


def getVariable(article):
    content = getContent(article)
    numOfreferences = getNumOfreferences(article)
    numOfCategories = getNumOfCategories(article)
    numOfLv2Heading = getNumOfLv2Heading(article)
    hasBox = hasInfoBox(article)
    nunOfCiteTemplates, numOfPageLinks, images = getNumOfCiteTemp(article)
    contentLength = getContentLength(content)
    imagesByLength = str(images / contentLength)
    flesch_reading_ease = getFlesch_reading_ease(content)
    coleman_liau_index = getColeman_liau_index(content)
    difficult_words = getDifficult_words(content)
    return (contentLength, numOfreferences, numOfPageLinks, nunOfCiteTemplates, numOfCategories, imagesByLength,
            hasBox, numOfLv2Heading,
            flesch_reading_ease, coleman_liau_index, difficult_words)


def getMaxValue(sunday, articles):
    for a in articles:
        kk = datetime.datetime.strptime(a['timestamp'], "%Y-%m-%dT%H:%M:%SZ")
        if kk <= sunday:
            return a
    return None


def write2Csv(articleNumber, articleName, obj):
    with open('result3/' + str(articleNumber) + '_' + articleName + '.csv', "w", newline='') as f:
        csv_writer = csv.writer(f)
        csv_writer.writerow(
            ['articleId', "articleName", "Y&W", "time", 'revisionID', 'contentLength', 'numOfreferences',
             'numOfPageLinks', 'nunOfCiteTemplates', 'numOfCategories', 'imagesByLength',
             'hasBox', 'numOfLv2Heading', 'flesch_reading_ease', 'coleman_liau_index', 'difficult_words', 'quality',
             'pageView'])
        for i in obj:
            csv_writer.writerow(i)
        f.close()


def job(q_result):
    while not q_result.empty():
        articleFileName = q_result.get()
        with open('result/' + articleFileName) as f:
            articleNumber = articleFileName.split('_')[0]
            articleName = articleFileName.split('_')[1]
            articleId = articleFileName.split('_')[2]
            ls = json.load(f)
            sundayList = getAllDayOfSunday()
            leatestRvid = None
            leatestData = None
            allPageview = getPageview(articleName, sundayList)
            qualityList = getQuality(ls)
            fileResult = []
            for s in sundayList:
                maxArticle = getMaxValue(s, ls)
                if maxArticle:
                    if leatestRvid != maxArticle['revid']:
                        result = getVariable(maxArticle['*'])
                        leatestRvid = maxArticle['revid']
                        leatestData = result
                    pageview = allPageview[s.strftime('%Y%m%d')]
                    ss = [articleId, articleName, s.strftime('%Yw%W'), s.strftime('%Y-%m-%d'), str(maxArticle['revid'])]
                    ss.extend(leatestData)
                    ss.append(qualityList[str(maxArticle['revid'])])
                    ss.append(pageview)
                    fileResult.append(ss)
                else:
                    ss = [articleId, articleName, s.strftime('%Yw%W'), s.strftime('%Y-%m-%d')]
                    fileResult.append(ss)
                if s.date() > datetime.date.today():
                    break
            write2Csv(articleNumber, articleName, fileResult)


def write2Csv2(articleNumber, articleName, obj):
    with open('result2/' + str(articleNumber) + '_' + articleName + '.csv', "w", newline='', encoding='utf-8') as f:
        csv_writer = csv.writer(f)
        csv_writer.writerow(
            ['articleId', "articleName", 'revid', 'dateOfWeek', 'time', 'userid', 'user', 'article'])
        for i in obj:
            csv_writer.writerow(i)
        f.close()


def job2(q_result):
    while not q_result.empty():
        articleFileName = q_result.get()
        with open('result/' + articleFileName) as f:
            articleNumber = articleFileName.split('_')[0]
            articleName = articleFileName.split('_')[1]
            articleId = articleFileName.split('_')[2]
            print(articleNumber, articleName)
            ls = json.load(f)
            result = []
            for l in ls:
                date_r = datetime.datetime.strptime(l['timestamp'], '%Y-%m-%dT%H:%M:%SZ')
                content = getContent(l.get('*', ''))
                result.append(
                    [articleId, articleName, l['revid'], date_r.strftime('%YW%W'),
                     date_r.strftime('%Y-%m-%d %H:%M:%S'),
                     l.get('userid', 'none'),
                     l.get('user', 'none'), content])
            write2Csv2(articleNumber, articleName, result)


def mergeCsv(filesName):
    wikiFiles = os.listdir(filesName)
    wikiFiles = sorted(wikiFiles, key=lambda x: int(x.split('_')[0]))
    print(wikiFiles)
    with open('0_allData2.csv', 'w', newline='', encoding='utf-8') as f:
        csv_writer = csv.writer(f)
        csv_writer.writerow(
            ['articleId', "articleName", 'revid', 'dateOfWeek', 'time', 'userid', 'user', 'article'])
        # csv_writer.writerow(
        #     ['articleId', "articleName", "Y&W", "time", 'revisionID', 'contentLength', 'numOfreferences',
        #      'numOfPageLinks', 'nunOfCiteTemplates', 'numOfCategories', 'imagesByLength',
        #      'hasBox', 'numOfLv2Heading', 'flesch_reading_ease', 'coleman_liau_index', 'difficult_words', 'quality',
        #      'pageView'])
        for w in wikiFiles:
            print(w)
            with open(filesName + '/' + w, encoding='utf-8') as f2:
                csv_read = csv.reader(f2)
                next(csv_read)
                for cr in csv_read:
                    csv_writer.writerow(cr)
            # csv_writer.writerow(i)
        f.close()


if __name__ == '__main__':
    mergeCsv('result2')

    # wikiFiles = os.listdir('result')
    # wikiFiles = sorted(wikiFiles, key=lambda x: int(x.split('_')[0]))
    # q_result = Queue()
    # for r in wikiFiles[136:]:
    #     q_result.put(r)
    # for i in range(1):
    #     p = Process(target=job2, args=(q_result,))
    #     p.start()

    # pageid, temp_result = getMainInfo('Synodontis_decorus')
    # getOne('360', 'Pierre Carbonnier')
