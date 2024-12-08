/**
 * 变更：
 *      1. 表【t_login_log】添加字段【login_ip_region】
 *
 */
 
 ALTER TABLE "t_login_log" 
  DROP COLUMN IF EXISTS "login_ip_region",
  ADD COLUMN "login_ip_region" varchar(255);