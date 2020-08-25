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
  admit_app AS (
      SELECT a.sfrstcr_term_code,
             a.sgbstdn_pidm,
             b.*
        FROM student_list a
   LEFT JOIN sabsupl b
          ON a.sgbstdn_pidm = b.sabsupl_pidm
       WHERE b.sabsupl_term_code_entry = (SELECT MAX(bb.sabsupl_term_code_entry)
                                            FROM sabsupl bb
                                           WHERE b.sabsupl_pidm = bb.sabsupl_pidm
                                             AND bb.sabsupl_term_code_entry <= a.sfrstcr_term_code)),
  credential_activity AS (
      SELECT a.sfrstcr_term_code,
             a.sgbstdn_pidm,
             SUM(CASE
                   WHEN b.shrdgmr_term_code_grad <= a.fall_up_one
                   THEN 1
                   ELSE 0
                   END) AS fall_up_one_grad,
             SUM(CASE
                   WHEN b.shrdgmr_term_code_grad <= a.fall_up_two
                   THEN 1
                   ELSE 0
                   END) AS fall_up_two_grad
        FROM student_list a
  INNER JOIN shrdgmr b
          ON a.sgbstdn_pidm = b.shrdgmr_pidm
    GROUP BY a.sfrstcr_term_code,
             a.sgbstdn_pidm),
  gpa_data AS (
      SELECT a.sfrstcr_term_code,
             a.sgbstdn_pidm,
             ROUND((SUM(b.shrtgpa_quality_points)/NULLIF(SUM(b.shrtgpa_gpa_hours),0)),2)AS year_one_cum_gpa,
             ROUND((SUM(c.shrtgpa_quality_points)/NULLIF(SUM(c.shrtgpa_gpa_hours),0)),2) AS year_two_cum_gpa
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

    SELECT a.sfrstcr_term_code AS term_code,
           a.sgbstdn_pidm AS student_pidm,
           TO_CHAR(c.spbpers_birth_date,'YYYYMMDD') AS birth_dt,
           c.spbpers_sex AS gender,
           CASE
             WHEN c.spbpers_citz_code = '2' THEN 'Nonresident alien'
             WHEN c.spbpers_ethn_cde = '2' THEN 'Hispanic'
             WHEN d.race_cdes LIKE '%H%' THEN 'Hispanic'
             WHEN d.race_cdes LIKE '%|%' THEN 'Two or more races'
             WHEN d.race_cdes IS NOT NULL THEN e.gorrace_desc
             ELSE 'Unknown'
             END AS race_desc,
           baninst1.gp_goksdif.f_get_sd_text('SPBPERS', 'FIRST_GEN_STUDENT', a.sgbstdn_pidm, 1) AS first_gen,
           b.hsgpact_hsgpact AS index_score,
           b.hsgpact_act AS act_score,
           f.sortest_test_score AS act_math,
           b.hsgpact_gpa AS hs_gpa,
           g.sorhsch_class_rank,
           g.sorhsch_class_size,
           g.sorhsch_percentile,
           h.stvsbgi_desc AS hs_name,
           j.sobsbgi_city AS hs_city,
           j.sobsbgi_stat_code AS hs_state,
           j.sobsbgi_zip AS hs_zip,
           CASE
             WHEN k.sabsupl_cnty_code_admit IN ('00030','00097','00099','030') THEN 'Unknown'
             WHEN k.sabsupl_cnty_code_admit IS NULL THEN 'Unknown'
             ELSE l.stvcnty_desc END AS home_utah_county,
           k.sabsupl_stat_code_admit AS home_state,
           m.stvnatn_nation AS home_country,
           n.spraddr_zip AS home_zip_code,
           CASE
             WHEN q.fall_up_one_grad > 0 THEN 'Graduated'
             WHEN o.sgbstdn_pidm IS NOT NULL THEN 'Retained'
             ELSE 'Did not retain'
             END AS fall_up_one,
           CASE
             WHEN q.fall_up_two_grad > 0 THEN 'Graduated'
             WHEN p.sgbstdn_pidm IS NOT NULL THEN 'Retained'
             ELSE 'Did not retain'
             END AS fall_up_two,
           r.year_one_cum_gpa,
           r.year_two_cum_gpa
           /* The following subquery limits population to first-time freshmen students. */
      FROM (SELECT aa.*
              FROM student_list aa
             WHERE aa.fterm_ind = 'Y'
               AND aa.sgbstdn_styp_code ='F') a
 LEFT JOIN dsc.hsgpact b
        ON a.sgbstdn_pidm = b.hsgpact_pidm
 LEFT JOIN spbpers c
        ON a.sgbstdn_pidm = c.spbpers_pidm
 LEFT JOIN (SELECT dd.gorprac_pidm,
                   LISTAGG(dd.gorprac_race_cde, '|') AS race_cdes
              FROM gorprac dd
          GROUP BY dd.gorprac_pidm) d
        ON a.sgbstdn_pidm = d.gorprac_pidm
 LEFT JOIN gorrace e
        ON d.race_cdes = e.gorrace_race_cde
 LEFT JOIN (SELECT ff.sortest_pidm,
                   MAX(ff.sortest_test_score) AS sortest_test_score
              FROM sortest ff
             WHERE ff.sortest_tesc_code IN ('A02N','A02')
          GROUP BY ff.sortest_pidm) f
        ON a.sgbstdn_pidm = f.sortest_pidm
 LEFT JOIN sorhsch g
        ON a.sgbstdn_pidm = g.sorhsch_pidm
 LEFT JOIN stvsbgi h
        ON g.sorhsch_sbgi_code = h.stvsbgi_code
 LEFT JOIN sobsbgi j
        ON g.sorhsch_sbgi_code = j.sobsbgi_sbgi_code
 LEFT JOIN admit_app k
        ON a.sgbstdn_pidm = k.sabsupl_pidm
       AND a.sfrstcr_term_code = k.sfrstcr_term_code
 LEFT JOIN stvcnty l
        ON k.sabsupl_cnty_code_admit = l.stvcnty_code
 LEFT JOIN stvnatn m
        ON k.sabsupl_natn_code_admit = m.stvnatn_code
 LEFT JOIN spraddr n
        ON a.sgbstdn_pidm = n.spraddr_pidm
       AND n.spraddr_atyp_code = '00'
       AND n.spraddr_seqno = '1'
 LEFT JOIN student_list o
        ON a.sgbstdn_pidm = o.sgbstdn_pidm
       AND a.fall_up_one = o.sfrstcr_term_code
 LEFT JOIN student_list p
        ON a.sgbstdn_pidm = p.sgbstdn_pidm
       AND a.fall_up_two = p.sfrstcr_term_code
 LEFT JOIN credential_activity q
        ON a.sgbstdn_pidm = q.sgbstdn_pidm
       AND a.sfrstcr_term_code = q.sfrstcr_term_code
 LEFT JOIN gpa_data r
        ON a.sgbstdn_pidm = r.sgbstdn_pidm
       AND a.sfrstcr_term_code = r.sfrstcr_term_code;
