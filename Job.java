package crawling;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.text.ParseException;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;

import org.apache.log4j.Logger;

import model.KeywordList;
import service.UrlParserService;
import util.CallException;
import util.DateFormatter;
import util.PropertyReader;
import util.TextSave;

public class Job {

	private static final Logger logger = Logger.getLogger(Job.class);

	@SuppressWarnings("static-access")
	public static void main(String[] args) throws CallException, UnsupportedEncodingException {
		

		String startDate = PropertyReader.getValue("collector.startDate");
		String end_Date = PropertyReader.getValue("collector.endDate");

		// csv에 설정한 키워드 리스트 가져오기
		List<HashMap<String, Object>> keywordList = KeywordList.getKeyWord();
		
		// LOG에 기록된 리스트 가져오기
		List<HashMap<String, Object>> logList = KeywordList.getLog();
		
		// 시작일 종료일 Data 타입 변수 선언
		Date beginDate = new Date(); // 시작일자
		Date endDate = new Date(); // 종료일자

		// 소요 되는 날짜
		long diff;

		// 최종 소요일 수
		int diffDays = 0; // 불러올 파일 일 수

		try {
			// 시작일
			beginDate = DateFormatter.dateFormatter(startDate);
			// 종료일
			endDate = DateFormatter.dateFormatter(end_Date);

			// 종료일 - 시작일
			diff = endDate.getTime() - beginDate.getTime();

			// Day로 변환
			diffDays = (int) (diff / (24 * 60 * 60 * 1000));

		} catch (ParseException e) {
			String msg = "날짜 계산 오류";
			throw new CallException(e, msg);
		}

		// 날짜 계산용 객체
		GregorianCalendar cal = new GregorianCalendar();

		// 시작 시간
		cal.setTime(beginDate);
		cal.add(cal.DAY_OF_MONTH, -1);
		
		logger.debug("수집 시작");
		System.out.println("수집 시작");


		for (int i = 0; i <= diffDays; i++) {
			cal.add(cal.DAY_OF_MONTH, 1);

			// 수집 데이터 일자
			Date run_date = cal.getTime();

			// 수집 데이터를 yyyymmdd 형식으로 변환
			String run_date_s;

			try {
				run_date_s = DateFormatter.dateFormatter(run_date);
			} catch (ParseException e1) {
				e1.printStackTrace();
				String msg = "날짜 변환 오류";
				throw new CallException(e1, msg);
			}

			for (HashMap<String, Object> keyword : keywordList) {

				String word;
				String keyWord = (String) keyword.get("keywordNm");
				String keyWordId = (String) keyword.get("keywordId");
				
				try {
					// 키워드를 UTF-8로 변환 시키기
					word = URLEncoder.encode(keyword.get("keywordNm").toString(), "UTF-8");
				} catch (UnsupportedEncodingException e) {
					String msg = "인코딩 오류";
					throw new CallException(e, msg);
				}


				try {	
					UrlParserService urlParserService = new UrlParserService();
					
					String[] param1 = { "CC0005", "CC0007", word, run_date_s, keyWord, keyWordId};
					//String[] param2 = { "CC0005", "CC0008", word, run_date_s, keyWord, keyWordId};
					String[] param3 = { "CC0006", "CC0007", word, run_date_s, keyWord, keyWordId};
					//String[] param4 = { "CC0006", "CC0008", word, run_date_s, keyWord, keyWordId};
					
					boolean isDate = urlParserService.isDate(logList, keyWordId, run_date_s);
					
					if (!isDate) {
						List<HashMap<String, Object>> naverBlog = urlParserService.getUrlParsing(param1);
						//urlParserService.getNaverCafeUrl(param2);
						List<HashMap<String, Object>> daumBlog = urlParserService.getUrlParsing(param3);
						//urlParserService.getUrlParsing(param4);
						
						try {
							TextSave ts = new TextSave();
							ts.textSave(naverBlog, param1);
							ts.textSave(daumBlog, param3);
							ts.saveLog(run_date_s, keyWordId);
						} catch (CallException e) {
							e.printStackTrace();
						}
						
					} else {
						System.out.println(run_date_s + ", "+ keyWordId + ", " + keyWord + " 데이터 수집된 기록이 있어 수집하지 않습니다.");
						logger.debug(run_date_s + ", "+ keyWordId + ", " + keyWord + " 데이터 수집된 기록이 있어 수집하지 않습니다.");
					}

				} catch (Exception e) {
					String msg = "데이터 수집 에러";
					throw new CallException(e, msg);
				}

			}

		}
		logger.debug("수집 종료");
		System.out.println("수집 종료");

		
	}
}
