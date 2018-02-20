var opbeans = new Object();

opbeans.GMap = function(mapDiv, opts, markers) {

	// Parameters
	this.gmapDiv = mapDiv;
	this.gmapOptions = opts;
	this.gmapMarkers = markers;

	// Member Object
	this.map = null;
	this.infoWin = new google.maps.InfoWindow();

	// Create Map
	if (this.gmapOptions.address) {
		this.getGeocode(this.gmapOptions.address);
	} else {
		this.createMap();
	}
};

opbeans.GMap.prototype.getGeocode = function(address) {

	var gmap = this;

	// 위치 검색
	var geocoder = new google.maps.Geocoder();
	geocoder.geocode({
		address : address
	}, function(results, status) {

		if (status == google.maps.GeocoderStatus.OK) {

			// 결과가 여러개 일 수 있다. (주소를 확실히 입력 받도록 처리해서 하나만 나오도록 하자
			// 여러개가 나올 경우, 무조건 첫번째 주소를 이용해서 Center 처리
			if (results.length >= 1) {
				gmap.createMap(results[0].geometry.location);
			}

		} else {
			alert("Not find Address : " + address + "(Reason:" + status + ")");
		}
	});
};

opbeans.GMap.prototype.createMap = function(center) {

	if (center) {
		this.gmapOptions.center = center;
	}

	// Map 생성
	this.initMap();

	// Marker 생성
	this.createMarker();
};

opbeans.GMap.prototype.initMap = function() {
	this.map = new google.maps.Map(this.gmapDiv, this.gmapOptions);
};

opbeans.GMap.prototype.createMarker = function() {

	// Marker 생성
	var type = Object.prototype.toString.apply(this.gmapMarkers);
	if (type == "[object Object]") {

	} else if (type == "[object String]") {
		this.gmapMarkers = eval('(' + this.gmapMarkers + ')');
	}

	for (var i = 0; i < this.gmapMarkers.markers.length; i++) {
		this.initMarker(i, this.gmapMarkers.markers[i]);
	}
};

opbeans.GMap.prototype.initMarker = function(index, markerInfo) {

	var code = String.fromCharCode("A".charCodeAt(0) + index);
	//var imgUrl = "Marker의 이미지를 설정할 수 있음.";

	var markerOption = {
		map : this.map,
		position : new google.maps.LatLng(markerInfo.lat, markerInfo.lng),
		zIndex : index + 1
		//icon : new google.maps.MarkerImage(imgUrl)
	};

	var marker = new google.maps.Marker(markerOption);
	this.setMarkerClickEvent(marker);
};

opbeans.GMap.prototype.setMarkerClickEvent = function(marker) {

	var gmap = this;

	// Marker 이벤트 생성
	google.maps.event.addListener(marker, 'click', function() {
		gmap.openInfoWindow(marker);
	});
};

opbeans.GMap.prototype.openInfoWindow = function(marker) {

	this.infoWin.close();
	this.infoWin.setContent(this.createInfoContent(marker));
	this.infoWin.open(marker.getMap(), marker);
};

opbeans.GMap.prototype.createInfoContent = function(marker) {

	// InfoWindow를 이용해 Marker의 상세 화면을 html을 이용해서 contents 데이터를 만들어 넣어주면 된다.
	return marker.getZIndex() + ":Marker";
};