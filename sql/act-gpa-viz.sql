WITH enrolled_students AS (
       SELECT DISTINCT a.sfrstcr_term_code,
              a.sfrstcr_pidm
         FROM sfrstcr a
        WHERE a.sfrstcr_camp_code <> 'XXX'
          AND a.sfrstcr_levl_code = 'UG'
          AND a.sfrstcr_rsts_code IN (SELECT b.stvrsts_code
                                        FROM stvrsts b
                                       WHERE b.stvrsts_incl_sect_enrl = 'Y')),
   student_list AS (
       SELECT a.sfrstcr_term_code,
              /*Create subsequent fall terms for comparison purposes.*/
              a.sfrstcr_term_code+100 AS fall_up_one,
              a.sfrstcr_term_code+200 AS fall_up_two,
              b.sgbstdn_pidm,
              b.sgbstdn_styp_code,
              CASE
                WHEN c.sgbstdn_pidm IS NOT NULL THEN 'Y'
                ELSE 'N'
                END AS fterm_ind
         FROM enrolled_students a
   INNER JOIN sgbstdn b
           ON a.sfrstcr_pidm = b.sgbstdn_pidm
          AND b.sgbstdn_stst_code = 'AS'
          AND SUBSTR(a.sfrstcr_term_code, 1, 4) BETWEEN '2012' AND '2020'
          AND SUBSTR(a.sfrstcr_term_code, 5, 2) = '40'
          AND b.sgbstdn_term_code_eff = (SELECT MAX(bb.sgbstdn_term_code_eff)
                                           FROM sgbstdn bb
                                          WHERE b.sgbstdn_pidm = bb.sgbstdn_pidm
                                            AND bb.sgbstdn_term_code_eff <= a.sfrstcr_term_code)
    LEFT JOIN sgbstdn c
           ON a.sfrstcr_pidm = c.sgbstdn_pidm
          AND c.sgbstdn_term_code_eff = a.sfrstcr_term_code
          AND b.sgbstdn_styp_code = c.sgbstdn_styp_code),
  gpa_data AS (
      SELECT a.sfrstcr_term_code,
             a.sgbstdn_pidm,
             ROUND((SUM(b.shrtgpa_quality_points)/NULLIF(SUM(b.shrtgpa_gpa_hours),0)),2)AS year_one_cum_dsu_gpa,
             ROUND((SUM(c.shrtgpa_quality_points)/NULLIF(SUM(c.shrtgpa_gpa_hours),0)),2) AS year_two_cum_dsu_gpa
        FROM student_list a
   LEFT JOIN shrtgpa b
          ON a.sgbstdn_pidm = b.shrtgpa_pidm
         AND b.shrtgpa_levl_code = 'UG'
         AND b.shrtgpa_gpa_type_ind = 'I'
         AND b.shrtgpa_term_code != '000000'
         AND b.shrtgpa_term_code < a.fall_up_one
   LEFT JOIN shrtgpa c
          ON a.sgbstdn_pidm = c.shrtgpa_pidm
         AND c.shrtgpa_levl_code = 'UG'
         AND b.shrtgpa_gpa_type_ind = 'I'
         AND b.shrtgpa_term_code != '000000'
         AND b.shrtgpa_term_code < a.fall_up_two
    GROUP BY a.sfrstcr_term_code,
             a.sgbstdn_pidm)

    SELECT a.sfrstcr_term_code AS first_term,
           a.sgbstdn_pidm AS student_pidm,
           b.sortest_test_score AS act_score,
           c.sortest_test_score AS act_math,
           d.sorhsch_gpa AS hs_gpa,
           e.year_one_cum_dsu_gpa,
           e.year_two_cum_dsu_gpa
      FROM (SELECT aa.sfrstcr_term_code,
                   aa.fall_up_one,
                   aa.fall_up_two,
                   aa.sgbstdn_pidm,
                   aa.sgbstdn_styp_code
              FROM student_list aa
             WHERE aa.fterm_ind = 'Y'
               AND aa.sgbstdn_styp_code ='F') a
 LEFT JOIN (SELECT bb.sortest_pidm,
                   MAX(bb.sortest_test_score) AS sortest_test_score
              FROM sortest bb
             WHERE bb.sortest_tesc_code IN ('A05')
          GROUP BY bb.sortest_pidm) b
        ON a.sgbstdn_pidm = b.sortest_pidm
 LEFT JOIN (SELECT cc.sortest_pidm,
                   MAX(cc.sortest_test_score) AS sortest_test_score
              FROM sortest cc
             WHERE cc.sortest_tesc_code IN ('A02N','A02')
          GROUP BY cc.sortest_pidm) c
        ON a.sgbstdn_pidm = c.sortest_pidm
 LEFT JOIN (SELECT dd.sorhsch_pidm,
                   dd.sorhsch_gpa,
                   dd.sorhsch_graduation_date,
                   ROW_NUMBER() OVER (
                     PARTITION BY dd.sorhsch_pidm
                     ORDER BY dd.sorhsch_graduation_date DESC) AS row_no
              FROM sorhsch dd) d
        ON a.sgbstdn_pidm = d.sorhsch_pidm
       AND d.row_no = 1
 LEFT JOIN gpa_data e
        ON a.sgbstdn_pidm = e.sgbstdn_pidm
       AND a.sfrstcr_term_code = e.sfrstcr_term_code;
