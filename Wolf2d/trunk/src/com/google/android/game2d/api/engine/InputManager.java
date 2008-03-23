package com.google.android.game2d.api.engine;

import java.util.ArrayList;
import java.util.List;

public class InputManager {
	public enum  Event {LEFT, RIGHT, UP, DOWN}
	
	private List<Event> events;
	
	private static InputManager instance;

	public static InputManager instance() {
		if (instance == null) {
			instance = new InputManager();
		}
		return instance;
	}

	private InputManager() {
		this.events = new ArrayList<Event>();
		
	}
	public void notifyEvent(Event event) {
		this.events.add(event);
	}
	
	public void reset() {
		this.events.clear();
	}
}
