package com.google.android.game2d.wolf;

import android.app.Activity;

import com.google.android.game2d.api.engine.EngineFactory;
import com.google.android.game2d.api.engine.GameEngine;

public class WolfFactory implements EngineFactory {

	private android.app.Activity activity;
	private static WolfFactory instance;
	private GameEngine engine;
	
	static void initialize(Activity activity) {
		if(instance == null) {
			instance = new WolfFactory(activity);
		}
	}
	
	public static WolfFactory instance() {
		return instance;
	}
	
	private WolfFactory(Activity activity) {
		this.activity = activity;
	}
	
	public GameEngine getEngine() {
		if(engine == null) {
			engine = new WolfEngine(activity); 
		}
		return engine;
	}

}
