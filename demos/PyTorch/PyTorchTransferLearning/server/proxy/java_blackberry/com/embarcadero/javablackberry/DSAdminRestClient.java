//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2021 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;


public class DSAdminRestClient {

	private DSRESTConnection Connection = null;

	protected DSRESTConnection getConnection() {
		return Connection;
	}

	public DSAdminRestClient(DSRESTConnection Connection) {
		super();
		this.Connection = Connection;
	}

}
