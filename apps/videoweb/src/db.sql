CREATE TABLE "public"."camera" (
	"id" int8 NOT NULL DEFAULT nextval('camera_id_seq'::regclass),
	"title" varchar(255) COLLATE "default",
	"location" varchar(255) COLLATE "default",
	"rtsp_url" varchar(255) COLLATE "default",
	"realtime_path" varchar(255) COLLATE "default",
	"analysis_on_time" time(6),
	"analysis_off_time" time(6),
	CONSTRAINT "camera_pkey" PRIMARY KEY ("id") NOT DEFERRABLE INITIALLY IMMEDIATE
)
WITH (OIDS=FALSE);
ALTER TABLE "public"."camera" OWNER TO "postgres";

CREATE TABLE "public"."video" (
	"id" int8 NOT NULL DEFAULT nextval('video_id_seq'::regclass),
	"camera_id" int8,
	"video_path" varchar(255) COLLATE "default",
	"poster_path" varchar(255) COLLATE "default",
	"start_date" timestamp(6) WITH TIME ZONE,
	"duration_in_second" int8,
	CONSTRAINT "video_pkey" PRIMARY KEY ("id") NOT DEFERRABLE INITIALLY IMMEDIATE,
	CONSTRAINT "fk_camera_id" FOREIGN KEY ("camera_id") REFERENCES "public"."camera" ("id") ON UPDATE NO ACTION ON DELETE NO ACTION NOT DEFERRABLE INITIALLY IMMEDIATE
)
WITH (OIDS=FALSE);
ALTER TABLE "public"."video" OWNER TO "postgres";
CREATE INDEX  "idx_camera_id" ON "public"."video" USING btree(camera_id ASC NULLS LAST);
CREATE UNIQUE INDEX  "video_id_key" ON "public"."video" USING btree("id" ASC NULLS LAST);

CREATE TABLE "public"."alert" (
	"id" int8 NOT NULL,
	"video_id" int8,
	"type" int8,
	"level" int8,
	"message" varchar(255) COLLATE "default",
	"poster_path" varchar(255) COLLATE "default",
	"start_date" timestamp(6) WITH TIME ZONE,
	"end_date" timestamp(6) WITH TIME ZONE,
	"end_reason" int8,
	CONSTRAINT "event_copy_pkey" PRIMARY KEY ("id") NOT DEFERRABLE INITIALLY IMMEDIATE,
	CONSTRAINT "alert_video_id_fkey" FOREIGN KEY ("video_id") REFERENCES "public"."video" ("id") ON UPDATE NO ACTION ON DELETE NO ACTION NOT DEFERRABLE INITIALLY IMMEDIATE
)
WITH (OIDS=FALSE);
ALTER TABLE "public"."alert" OWNER TO "postgres";
CREATE INDEX  "idx_alert_video_id" ON "public"."alert" USING btree(video_id ASC NULLS LAST);

CREATE TABLE "public"."event" (
	"id" int8 NOT NULL DEFAULT nextval('event_id_seq'::regclass),
	"video_id" int8,
	"create_date" timestamp(6) WITH TIME ZONE DEFAULT now(),
	"top_x" int8,
	"top_y" int8,
	"width" int8,
	"height" int8,
	"type" int8,
	CONSTRAINT "event_pkey" PRIMARY KEY ("id") NOT DEFERRABLE INITIALLY IMMEDIATE,
	CONSTRAINT "fk_video_id" FOREIGN KEY ("video_id") REFERENCES "public"."video" ("id") ON UPDATE NO ACTION ON DELETE NO ACTION NOT DEFERRABLE INITIALLY IMMEDIATE
)
WITH (OIDS=FALSE);
ALTER TABLE "public"."event" OWNER TO "postgres";
CREATE INDEX  "idx_video_id" ON "public"."event" USING btree(video_id ASC NULLS LAST);
