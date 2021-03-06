/* Copyright 1984-1992 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 *
 *  MUST FIT IN UNSGNE CHAR - 255 MAX!
 */

# define PLAY_IN_EMPTY_CORNER 0
# define MAKE_34_POINTS_SPIRAL 1
# define PLAY_NEXT_TO_44_POINT_STONE 2
# define PLAY_IN_CORNER_POINT 3
# define POINT_AT_EMPTY 4
# define DONT_POINT_AT_OPP 5
# define JOSEKI 6
# define PREVENT_2_SHIMARI 7
# define SHIMARI_OR_44_POINT 8
# define STONE_FACING 9
# define MAKE_SHIMARI 10
# define DOUBLE_ATARI 11
# define PUSH_THROUGH 12
# define CAN_CUT 13
# define CONNECT_THRU 14
# define NO_RULE_APPLIES 15
# define BLOCKING_MOVE 16
# define TRY_SAVE_WEAK 17
# define EXTEND_STRENGTHEN_GROUP 18
# define SKIP_ALONG_EDGE 19
# define SKIP_FROM_MT_TRI 20
# define BLOCK 21
# define ATARI_KOTHREAT 22
# define EXTEND_TO_ENEMY 23
# define EXTEND_TO_4POINT 24
# define INVADE_WITH_ROOM 25
# define PLAY_HIGH_FROM_LOW 26
# define BIG_MOVE 27
# define DEFEND_MOYO 28
# define PREVENT_2_WING 29
# define GROUP_DEAD 30
# define PUT_IN_ATARI 31
# define DONT_MAKE_DEAD_GROUPS 32
# define BLOCK_FROM_TERR 33
# define CUT_AT_EDGE 34
# define DONT_ADD_TO_DEAD_GROUP 35
# define ILLEGAL_KO 36
# define ILLEGAL_STONE 37
# define ATTACK_GROUP 38
# define PATCONNDEF 39
# define RUNCUTS 40
# define TRY_TO_KILL 41
# define FILLDAME 42
# define EMPTY_TRIANGLE 43
# define GET_MORE_LIBS 44
# define TAKE_AWAY_LIBS 45
# define CRAWL_UNDER 46
# define CRAWL_OVER 47
# define BLOCK_AWAY 48
# define TOWARD_FRIEND 49
# define FILL_HOLE 50
# define DOUBLE_ATARI1 51
# define SENTE_CUT 52
# define SKIP_FOR_BLOCK 53
# define NO_SUICIDE 54
# define NO_DEAD_BLOCK 55
# define NO_TH_GROUPS 56
# define KOTHREAT 57
# define SAVE_TH_GROUP 58
# define SAVE_CUTTING_STONES 59
# define POKE_EYE 60
# define TRY_ATARI 61
# define CAPTURE_THREATENED 62
# define CAPTURE_CUTTING_STONES 63
# define PRO_MOVE 64
# define SENTE_THREAT_MOVE 65
# define SENTE_THREAT_CONN 66
# define SAVE_GROUP_ATTACK 67
# define CAPTURE_DEF_WEAK 68
# define REALLY_NOT_TO_EDGE 69
# define SAVE_THREATENED_GROUP 70
# define INVADE_NEAR_WEAKER_GROUP 71
# define PLAY_NEAR_WEAKER_GROUP 72
# define EXTEND_AND_ATTACK 73
# define ATTACK_WITH_CAP 74
# define POKE_EYE_KO 75
# define SAVE_VITAL_UNUSED 76
# define FILLDAME_NEUTRAL 77
# define FILLDAME_CONNECT 78
# define SAVE_UNSETTLED_VITAL_UNUSED 79
# define SAVE_UNSETTLED_GROUP 80
# define NO_GOOD_REASON 81
# define EXTEND_UNSETTLED_GROUP 82
# define EXTEND_WEAK_GROUP 83
# define EXTEND_ATTACK_UNSETTLED 84
# define INVADE_ATTACK 85
# define ATTACK_WITH_KOSUMI 86
# define URG_JOSEKI 87
# define COMP_JOSEKI 88
# define BAD_JOSEKI 89
# define KILL_UNSETTLED_GROUP 90
# define SHIM_JOSEKI 91
# define FOLL_JOSEKI 92
# define TRIK_JOSEKI 93
# define FUSEKI_LIB 94
# define THRT_UNSETTLED_GROUP 95
# define TAKE_CORNER 96
# define PROTECT_CORNER 97
# define DONT_TRY_TO_SAVE 98
# define RUN_AWAY 99
# define TAKE_OFF_BOARD 100
# define DONT_MK_THR 101
# define SAVED_GROUP 102
# define SINGLE_CONNECTION 103
# define DONT_MK_UNS 104
# define REPETITION 105
# define PLAY_IN_CENTER 106
# define WINNING_MOVE 107
# define EXTEND_FROM_SHIM 108
# define SHIM_INVADE 109
# define INVADE_UNDER_4 110
# define MAYBE_JOSEKI 111
# define TRY_33_INV 112
# define JOSEKI_NO_TENUKI 113
# define MAKE_SAN_REN_SEI 114
# define LOSING_MOVE 115
# define DEFEND_EDGE_TERR 116
# define KNIGHTS_TO_EDGE 117
# define REMOVE_DEAD 118
# define MAKE_EYE_SHAPE 119
# define WIN_SEMEAI_VITAL_UNUSED 120
# define SEMEAI_NBR_VITAL 121
# define SEMEAI_MORE_LIBS 122
# define SEMEAI_OUTSIDE_LIB 123
# define SEMEAI_INSIDE_LIB 124
# define DEFEND_EYE_SPACE 125
# define CONNECT_OUT 126
# define ATTACK_SURROUND 127
# define COVER_WEAKNESS 128
# define SHIMARI_EXTEND 129
# define TWO_SHIMARI_EXTEND 130
# define SAFE_CAPTURE 131
# define BEHIND_VITAL 132
# define TOWARD_STRENGTH 133
# define SQUIRM_CAPTURE 134
# define CONNECT_BAMBOO 135
# define CAPTURE_SAFE 136
# define BLOCK_DEF_TERR 137
# define CUT_WIN_SEMEAI 138
# define INVADE_BEHIND 139
# define CONNECT_KNIGHTS 140
# define SECTOR_DEFEND 141
# define PLAY_44_2_EMPTY 142
# define KAKARI 143
# define NI_REN_SEI 144
# define INVADE_BEHIND_WEAK 145
# define POINT_AT_FRIENDLY_44 146
# define CENTER_FOR_7_LINE 147
# define NOT_JOSEKI 148
# define ATTACK_EYE_POT_UNUSED 149
# define UNUSED150 150
# define UNUSED151 151
# define UNUSED152 152
# define SAFE_STRENGTHEN_GROUP 153
# define WORSE_THAN_PASS 154
# define DOUBLE_THREAT 155
# define INTO_ENEMY_TERR 156
# define BLOCK_ENEMY_TERR 157
# define ATTACK_CUT_UNUSED 158
# define SEMEAI_MAKEEYE 159
# define PINCER_EXTENSION 160
# define ATTACK_VITAL_UNUSED 161
# define SEMEAI_KILL_UNS_UNUSED 162
# define SEMEAI_CONNECT 163
# define SEMEAI_CUT 164
# define SEMEAI_VITAL 165
# define SEMEAI_SAVETHR 166
# define SEMEAI_EYESPACE 167
# define ENDPATMATCH 168
# define PATTERN_SEQUENCE 169
# define OPPONENT_RESPOND 170
# define OPP_SENTE 171
# define OPP_KILL 172
# define OPP_SAVE 173
# define EYEPOT_SAVE_UNSETTLED 174
# define OBVIOUSANSWER 175
# define CHINESE_STYLE 176
# define PATMATCH 177
# define PATCUT 178
# define PATCONNECT 179
# define PATSEQUENCE 180
# define PATSURROUND 181
# define PATRUN 182
# define PATKILL 183
# define PATSAVE 184
# define PATINVADE 185
# define PATDEFEND 186
# define ATTACK_NOEXTEND_UNUSED 187
# define ATK_TO_LIVE 188
# define MAKE_LIFE 189
# define KILL_GROUP 190
# define ATTACK_VITAL_SAC_UNUSED 191
# define MAKE_LIFE2_UNUSED 192
# define KILL_GROUP2 193
# define INVADE_HIGH 194
# define KILL_TO_SAVE 195
# define PATINVSEQUENCE 196
# define PATCUTSEQUENCE 197
# define MAKE_GROUP_LIVE 198
# define MAKE_KO_LIFE 199
# define CANT_LIVE 200
# define MAKE_GROUP_DIE 201
# define MAKE_KO_DIE 202
# define CANT_KILL 203
# define PATKILLCUTSTONES 204
# define KILL_TO_LIVE 205
# define KILL_CUTTING 206
# define KO_CUTTING 207
# define SAVE_CUTTING 208
# define SAVE_KO_CUTTING 209
# define TRY_TO_SAVE 210
# define SAFE_CONNECT 211
# define OPP_BIG 212
# define NOTURGENT 213
# define KILL_WORKS 214
# define LIVE_WORKS 215


 




