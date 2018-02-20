library(networkD3)

EvTopic <- list(name = "배터리", children = list(list(name = "충전", children = list(list(name = "구축"),
                                                                                list(name = "이산화탄소"),
                                                                                list(name = "설치"),
                                                                                list(name = "급속"),
                                                                                list(name = "완속"))),
                                              list(name = "주행거리", children = list(list(name = "문제"),
                                                                                  list(name = "효율"),
                                                                                  list(name = "고속도로"),
                                                                                  list(name = "연비"))),
                                              list(name = "시간", children = list(list(name = "충전"))),
                                              list(name = "용량", children = list(list(name = "보증"),
                                                                                list(name = "교체"),
                                                                                list(name = "급속")))))

diagonalNetwork(List = EvTopic, fontSize = 30, fontFamily = "돋움체",linkColour = "red", opacity = 1)
